-record(prom_help, {
    name :: string(),
    text :: undefined | string()
    }).
-record(prom_type, {
    name :: string(),
    type :: counter | gauge | histogram | summary | untyped
    }).
-record(prom_comment, {
    text :: string()
    }).
-record(prom_value, {
    name :: string(),
    labels = #{} :: #{binary() => binary()},
    value :: '+Inf' | '-Inf' | float(),
    timestamp :: undefined | integer()
    }).
