use std::sync::Arc;

use juniper::{DefaultScalarValue, EmptyMutation, EmptySubscription, RootNode, Variables, graphql_value};

use crate::{Node, NodeSchema, NodeTypeInfo};

#[test]
fn test_happy() {
    let node_schema: Arc<NodeSchema> = Arc::new(crate::parse_graphql_schema(r#"
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
    let node_schema: Arc<NodeSchema> = Arc::new(crate::parse_graphql_schema(r#"
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
    let node_schema: Arc<NodeSchema> = Arc::new(crate::parse_graphql_schema(r#"
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
