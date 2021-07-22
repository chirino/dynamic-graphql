use juniper::{GraphQLType, GraphQLValue, ScalarValue, Registry, Arguments, Executor, ExecutionResult, EmptyMutation, RootNode, EmptySubscription, Variables, DefaultScalarValue, FieldError};
use juniper::meta::{MetaType, Field};
use serde_json::{Value, Map};
use graphql_parser::schema::{Definition, TypeDefinition, Text};
use std::collections::HashMap;
use std::sync::Arc;
use graphql_parser::query::Type;
use juniper::graphql_value;

#[derive(Debug, Clone, PartialEq)]
struct NodeSchema
{
    pub types: Vec<NodeType>,
    pub query_type_ref: Option<NodeType>,
    pub mutation_type_ref: Option<NodeType>,
    pub subscription_type_ref: Option<NodeType>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeType where {
    type_idx: usize,
    name: String,
    fields: HashMap<String, TypeRef>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeRef {
    NamedType(usize),
    ListType(Box<TypeRef>),
    NotNullType(Box<TypeRef>),
}

fn parse_graphql_schema(schema_txt: &str) -> Result<NodeSchema> {
    use graphql_parser::schema;
    use schema::parse_schema;

    let ast = parse_schema::<String>(schema_txt)?.to_owned();

    let mut node_schema = NodeSchema {
        types: Vec::new(),
        query_type_ref: None,
        mutation_type_ref: None,
        subscription_type_ref: None,
    };

    // 1. create entries for all the types defined in the schema..

    let mut types_by_name = HashMap::new();
    let node_type = NodeType {
        type_idx: 0,
        name: "String".to_string(),
        fields: HashMap::new(),
    };
    types_by_name.insert(node_type.name.clone(), node_type.type_idx);
    node_schema.types.push(node_type);

    let node_type = NodeType {
        type_idx: 1,
        name: "Int".to_string(),
        fields: HashMap::new(),
    };
    types_by_name.insert(node_type.name.clone(), node_type.type_idx);
    node_schema.types.push(node_type);
    let mut idx: usize = 1;

    for d in &ast.definitions {
        match d {
            Definition::TypeDefinition(d) => {
                match d {
                    TypeDefinition::Object(d) => {
                        let type_idx = {
                            idx += 1;
                            idx
                        };
                        let node_type = NodeType {
                            type_idx,
                            name: d.name.clone(),
                            fields: HashMap::new(),
                        };
                        types_by_name.insert(node_type.name.clone(), node_type.type_idx);
                        node_schema.types.push(node_type);
                    }
                    _ => {
                        panic!("not implemented");
                    }
                }
            }
            _ => {}
        }
    }


    for d in &ast.definitions {
        match &d {
            // 2. add the fields to the types, now that we can lookup types by name..
            Definition::TypeDefinition(d) => {
                match d {
                    TypeDefinition::Object(d) => {
                        let node_type_idx = *types_by_name.get(d.name.as_str()).unwrap();
                        let node_type = &mut node_schema.types[node_type_idx];

                        for field in &d.fields {
                            node_type.fields.insert(field.name.to_string(), get_type_ref(&mut types_by_name, &field.field_type));
                        }
                    }
                    _ => panic!("not implemented"),
                }
            }
            _ => {}
        }
    }

    for d in &ast.definitions {
        match &d {
            // 3. find the root schema types
            Definition::SchemaDefinition(d) => {
                let d = d.clone();
                if d.query.is_some() {
                    let type_name = d.query.unwrap().clone();
                    node_schema.query_type_ref = types_by_name.get(type_name.as_str()).map(|x| node_schema.types.get(*x).unwrap().clone())
                }
                if d.mutation.is_some() {
                    let type_name = d.mutation.unwrap().clone();
                    node_schema.mutation_type_ref = types_by_name.get(type_name.as_str()).map(|x| node_schema.types.get(*x).unwrap().clone())
                }
                if d.subscription.is_some() {
                    let type_name = d.subscription.unwrap().clone();
                    node_schema.subscription_type_ref = types_by_name.get(type_name.as_str()).map(|x| node_schema.types.get(*x).unwrap().clone())
                }
            }
            _ => {}
        }
    }

    Ok(node_schema)
}

fn get_type_ref<'a, T>(types_by_name: &mut HashMap<String, usize>, field_type: &Type<'a, T>) -> TypeRef where
    T: Text<'a>
{
    match field_type {
        Type::NamedType(field_type_name) => {
            let field_name = field_type_name.as_ref().to_string();
            match types_by_name.get(&field_name) {
                Some(i) => TypeRef::NamedType(*i),
                None => { panic!("type {} not defined in schema", field_name); }
            }
        }
        Type::ListType(x) => {
            TypeRef::ListType(Box::new(get_type_ref(types_by_name, x)))
        }
        Type::NonNullType(x) => {
            TypeRef::NotNullType(Box::new(get_type_ref(types_by_name, x)))
        }
    }
}

impl TypeRef {
    fn is_nullable(&self) -> (bool, &TypeRef) {
        match self {
            TypeRef::NotNullType(x) => (false, x.as_ref()),

            _ => (true, self)
        }
    }
}


impl Node {
    fn build_field<'r, S>(info: &NodeTypeInfo, registry: &mut Registry<'r, S>, field_name: &String, field_type_ref: &TypeRef) -> Field<'r, S> where
        S: ScalarValue,
    {
        let (nullable, type_ref) = field_type_ref.is_nullable();
        match type_ref {
            TypeRef::NamedType(idx) => {
                match *idx {
                    0 => {
                        if nullable {
                            registry.field::<Option<String>>(field_name, &())
                        } else {
                            registry.field::<String>(field_name, &())
                        }
                    }
                    1 => {
                        if nullable {
                            registry.field::<Option<i32>>(field_name, &())
                        } else {
                            registry.field::<i32>(field_name, &())
                        }
                    }
                    _ => {
                        let field_node_type = info.schema.types.get(*idx).unwrap();
                        let field_node_type_info = &NodeTypeInfo {
                            schema: info.schema.clone(),
                            node_type: field_node_type.clone(),
                        };
                        if nullable {
                            registry.field::<Option<Node>>(field_name, field_node_type_info)
                        } else {
                            registry.field::<Arc<Node>>(field_name, field_node_type_info)
                        }
                    }
                }
            }
            TypeRef::ListType(_) => panic!("implement me "),
            TypeRef::NotNullType(_) => panic!("implement me "),
        }
    }
}


pub struct NodeTypeInfo {
    schema: Arc<NodeSchema>,
    node_type: NodeType,
}

pub struct Node {
    fields: Map<String, Value>,
}

impl<S> GraphQLType<S> for Node
    where
        S: ScalarValue,
{
    fn name(info: &Self::TypeInfo) -> Option<&str> {
        Some(info.node_type.name.as_str())
    }

    fn meta<'r>(info: &Self::TypeInfo, registry: &mut Registry<'r, S>) -> MetaType<'r, S>
        where
            S: 'r,
    {
        let mut fields = Vec::new();
        info.node_type.fields.iter().for_each(|(field_name, field_type_ref)| {
            let field = <Node>::build_field(info, registry, field_name, field_type_ref);
            fields.push(field);
        });
        registry
            .build_object_type::<Node>(info, &fields)
            .into_meta()
    }
}


impl<S> GraphQLValue<S> for Node
    where
        S: ScalarValue,
{
    type Context = ();
    type TypeInfo = NodeTypeInfo;

    fn type_name<'i>(&self, info: &'i Self::TypeInfo) -> Option<&'i str> {
        <Self as GraphQLType<S>>::name(info)
    }

    fn resolve_field(
        &self,
        info: &Self::TypeInfo,
        field_name: &str,
        _args: &Arguments<S>,
        executor: &Executor<Self::Context, S>,
    ) -> ExecutionResult<S> {
        let field_value = self.fields.get(field_name);
        let field_type = info.node_type.fields.get(field_name).unwrap();
        let (nullable, type_ref) = field_type.is_nullable();

        match type_ref {
            TypeRef::NamedType(idx) => {
                match *idx {
                    0 => {
                        let value = field_value.map(|x| x.as_str().map(|x| x.to_string())).flatten();
                        if nullable {
                            executor.resolve::<Option<String>>(&(), &value)
                        } else {
                            match value {
                                Some(x) => executor.resolve::<String>(&(), &x),
                                None => Err(FieldError::new(format!("required field {} is null", field_name), juniper::Value::Null))
                            }
                        }
                    }
                    1 => {
                        let value = field_value.map(|x| x.as_i64().map(|x| x as i32)).flatten();
                        if nullable {
                            executor.resolve::<Option<i32>>(&(), &value)
                        } else {
                            match value {
                                Some(x) => executor.resolve::<i32>(&(), &x),
                                None => Err(FieldError::new(format!("required field {} is null", field_name), juniper::Value::Null))
                            }
                        }
                    }
                    _ => {
                        let value = field_value.map(|x| x.as_object().map(|x| Node {
                            fields: x.clone(),
                        })).flatten();

                        let field_node_type = info.schema.types.get(*idx).unwrap();
                        let field_node_type_info = &NodeTypeInfo {
                            schema: info.schema.clone(),
                            node_type: field_node_type.clone(),
                        };
                        if nullable {
                            executor.resolve::<Option<Node>>(field_node_type_info, &value)
                        } else {
                            match value {
                                Some(x) => executor.resolve::<Node>(field_node_type_info, &x),
                                None => Err(FieldError::new(format!("required field {} is null", field_name), juniper::Value::Null))
                            }
                        }
                    }
                }
            }
            TypeRef::ListType(_) => {
                todo!()
            }
            TypeRef::NotNullType(_) => {
                todo!()
            }
        }
    }
}

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;


#[test]
fn test_happy() {
    let node_schema: Arc<NodeSchema> = Arc::new(parse_graphql_schema(r#"
        schema {
            query: Example
        }
        type Example {
            foo: String,
            bar: Int,
            baz: Example,
            foo_r: String!,
            bar_r: Int!,
            baz_r: Example!,
        }
    "#).unwrap());

    let data = r#"
        {
            "foo": "1",
            "bar": 2,
            "baz": {
                "foo": "3",
                "bar": 4
            },
            "foo_r": "5",
            "bar_r": 6,
            "baz_r": {
                "foo": "7"
            }
        }"#;

    let schema: RootNode<_, _, _> = RootNode::new_with_info(
        Node { fields: serde_json::from_str(data).unwrap() },
        EmptyMutation::new(),
        EmptySubscription::new(),
        NodeTypeInfo {
            schema: node_schema.clone(),
            node_type: node_schema.query_type_ref.clone().unwrap(),
        },
        (),
        (),
    );


    let actual: juniper::Value<DefaultScalarValue> = juniper::execute_sync(
        r#"
            {
                foo
                bar
                baz {
                  foo
                  bar
                }
                foo_r
                bar_r
                baz_r {
                  foo
                }
            }"#,
        None,
        &schema,
        &Variables::new(),
        &()).unwrap().0;

    let expected: juniper::Value<DefaultScalarValue> = graphql_value!({
        "foo": "1",
        "bar": 2,
        "baz": {
            "foo": "3",
            "bar": 4
        },
        "foo_r": "5",
        "bar_r": 6,
        "baz_r": {
            "foo": "7",
        }
    });

    assert_eq!(expected, actual);
}

#[test]
fn test_no_data() {
    let node_schema: Arc<NodeSchema> = Arc::new(parse_graphql_schema(r#"
        schema {
            query: Example
        }
        type Example {
            foo: String,
            bar: Int,
            baz: Example,
            foo_r: String!,
            bar_r: Int!,
            baz_r: Example!,
        }
    "#).unwrap());

    let data = r#"{}"#;

    let schema: RootNode<_, _, _> = RootNode::new_with_info(
        Node { fields: serde_json::from_str(data).unwrap() },
        EmptyMutation::new(),
        EmptySubscription::new(),
        NodeTypeInfo {
            schema: node_schema.clone(),
            node_type: node_schema.query_type_ref.clone().unwrap(),
        },
        (),
        (),
    );

    let (actual, errs) = juniper::execute_sync(
        r#"
            {
                #foo
                #bar
                #baz { foo, bar}
                foo_r
                bar_r
                baz_r { foo, bar}
            }"#,
        None,
        &schema,
        &Variables::new(),
        &()).expect("Execution failed");

    assert_eq!(errs.len(), 1);
    assert_eq!(actual, graphql_value!(None));
}


#[test]
fn test_null_data() {
    let node_schema: Arc<NodeSchema> = Arc::new(parse_graphql_schema(r#"
        schema {
            query: Example
        }
        type Example {
            foo: String,
            bar: Int,
            baz: Example,
            foo_r: String!,
            bar_r: Int!,
            baz_r: Example!,
        }
    "#).unwrap());
    let data = r#"
        {
            "foo": null,
            "bar": null,
            "baz": null,
            "foo_r": null,
            "bar_r": null,
            "baz_r": null
        }"#;

    let schema: RootNode<_, _, _> = RootNode::new_with_info(
        Node { fields: serde_json::from_str(data).unwrap() },
        EmptyMutation::new(),
        EmptySubscription::new(),
        NodeTypeInfo {
            schema: node_schema.clone(),
            node_type: node_schema.query_type_ref.clone().unwrap(),
        },
        (),
        (),
    );

    let (actual, errs) = juniper::execute_sync(
        r#"
            {
                #foo
                #bar
                #baz { foo, bar}
                foo_r
                bar_r
                baz_r { foo, bar}
            }"#,
        None,
        &schema,
        &Variables::new(),
        &()).expect("Execution failed");

    assert_eq!(errs.len(), 1);
    assert_eq!(actual, graphql_value!(None));
}

