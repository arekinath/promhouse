-record(prom_help, {
    name :: binary(),
    text :: undefined | binary()
    }).
-record(prom_type, {
    name :: binary(),
    type :: counter | gauge | histogram | summary | untyped
    }).
-record(prom_comment, {
    text :: binary()
    }).
-record(prom_metric_value, {
    name :: binary(),
    labels :: #{binary() => binary()},
    value :: '+Inf' | '-Inf' | float(),
    timestamp :: undefined | integer()
    }).
