module.exports = grammar({
  name: "yack",

  externals: ($) => [$.comment],

  extras: ($) => [/\s/, $.comment],

  word: ($) => $.identifier,

  conflicts: ($) => [
    [$.borrow_type],
    [$.qualified_identifier, $.primary_expression],
    [$.qualified_identifier, $.primary_expression, $.qualified_constructor],
    [$.case_branch, $.binary_expression],
    [$.type, $.function_type],
    [$.lambda_parameter_list, $.unit_expression],
    [$.lambda_parameter, $.primary_expression],
    [$.lambda_expression, $.primary_expression],
    [$.primary_expression, $.pattern],
    [$.primary_expression, $.variable_pattern],
    [$.record_expression, $.record_pattern],
    [$.list_expression, $.list_pattern],
    [$.lambda_parameter, $.primary_expression, $.variable_pattern],
  ],

  rules: {
    source_file: ($) => repeat($._declaration),

    _declaration: ($) =>
      choice(
        $.import_declaration,
        $.function_declaration,
        $.type_declaration,
        $.datatype_declaration,
      ),

    import_declaration: ($) =>
      seq(
        "import",
        field("module", $.module_path),
        optional($.import_list),
        optional(seq("as", field("alias", $.identifier))),
      ),

    module_path: ($) =>
      seq($.identifier, repeat(seq("/", $.identifier))),

    import_list: ($) =>
      seq(
        ".",
        "{",
        optional(seq($.import_item, repeat(seq(",", $.import_item)))),
        "}",
      ),

    import_item: ($) =>
      choice(
        seq("type", field("name", $.identifier), optional(seq("as", field("alias", $.identifier)))),
        seq(field("name", $.identifier), optional(seq("as", field("alias", $.identifier)))),
      ),

    function_declaration: ($) =>
      seq(
        "fun",
        field("name", $.identifier),
        field("parameters", $.parameter_list),
        optional(seq(":", field("return", $.type))),
        "=",
        field("body", $.expression),
      ),

    parameter_list: ($) =>
      seq(
        "(",
        optional(seq($.parameter, repeat(seq(",", $.parameter)))),
        ")",
      ),

    parameter: ($) =>
      seq(
        field("pattern", $.pattern),
        optional(seq(":", field("type", $.type))),
      ),

    type_declaration: ($) =>
      seq(
        "type",
        optional(field("parameters", $.type_parameters)),
        field("name", $.identifier),
        optional(seq(":", $.property)),
        "=",
        field("body", $.type),
      ),

    property: ($) => choice("Copy", "Affine", "Linear"),

    record_type: ($) =>
      seq(
        "{",
        optional(seq($.field_declaration, repeat(seq(",", $.field_declaration)))),
        "}",
      ),

    field_declaration: ($) =>
      seq(
        field("name", $.identifier),
        ":",
        field("type", $.type),
      ),

    datatype_declaration: ($) =>
      seq(
        "datatype",
        optional(field("parameters", $.type_parameters)),
        field("name", $.identifier),
        optional(seq(":", $.property)),
        "=",
        field("first", $.constructor_declaration),
        repeat(seq("|", $.constructor_declaration)),
      ),

    type_parameters: ($) =>
      choice(
        $.type_variable,
        seq($.type_variable, repeat1($.type_variable)),
        seq(
          "(",
          seq($.type_variable, repeat(seq(",", $.type_variable))),
          ")",
        ),
      ),

    constructor_declaration: ($) =>
      seq(
        field("name", $.constructor),
        optional(seq("of", field("type", $.type))),
      ),

    type: ($) => choice($.function_type, $._tuple_type),

    function_type: ($) =>
      prec.right(
        seq(
          field("parameters", $._tuple_type),
          "->",
          field("result", $.type),
        ),
      ),

    _tuple_type: ($) =>
      prec.left(
        seq($._application_type, repeat(seq("*", $._application_type))),
      ),

    _application_type: ($) =>
      prec.left(
        seq($._atomic_type, repeat($._atomic_type)),
      ),

    _atomic_type: ($) =>
      choice(
        $.primitive_type,
        $.qualified_type,
        $.type_identifier,
        $.type_variable,
        $.borrow_type,
        $.consume_type,
        $.parenthesized_type,
        $.record_type,
      ),

    consume_type: ($) =>
      prec.right(
        seq("consume", field("inner", $._application_type)),
      ),

    primitive_type: ($) =>
      choice(
        "int",
        "int1",
        "int2",
        "int4",
        "int8",
        "int16",
        "int32",
        "int64",
        "float",
        "float32",
        "float64",
        "bool",
        "unit",
        "token",
        "resource",
        "string",
      ),

    type_identifier: ($) => $.identifier,

    qualified_type: ($) =>
      seq($.identifier, ".", choice($.identifier, $.constructor)),

    type_variable: ($) =>
      token(seq(choice("''", "'"), /[A-Za-z_][A-Za-z0-9_']*/)),

    borrow_type: ($) =>
      seq(
        choice("borrow", "&"),
        optional("mut"),
        field("inner", $.type),
        optional(seq("from", choice($.integer, $.identifier))),
      ),

    parenthesized_type: ($) =>
      seq(
        "(",
        $.type,
        repeat(seq(",", $.type)),
        ")",
      ),

    expression: ($) =>
      choice(
        $.let_expression,
        $.arena_expression,
        $.if_expression,
        $.case_expression,
        $.lambda_expression,
        $.borrow_expression,
        $.binary_expression,
        $.unary_expression,
        $.qualified_call_expression,
        $.postfix_expression,
        $.primary_expression,
      ),

    qualified_identifier: ($) =>
      seq($.identifier, repeat1(seq(".", $.identifier))),

    qualified_call_expression: ($) =>
      prec.dynamic(
        2,
        prec.left(
          5,
          seq(
            field("function", $.qualified_identifier),
            field("arguments", $.implicit_arguments),
          ),
        ),
      ),

    implicit_arguments: ($) => repeat1($.postfix_expression),

    let_expression: ($) =>
      seq(
        "let",
        repeat1($._binding),
        "in",
        field("body", $.expression),
        "end",
      ),

    arena_expression: ($) =>
      seq(
        "arena",
        repeat1($._binding),
        "in",
        field("body", $.expression),
        "end",
      ),

    _binding: ($) => choice($.value_binding, $.function_binding, $.guard_binding),

    guard_binding: ($) =>
      choice($.guard_pattern, $.guard_condition),

    guard_condition: ($) =>
      seq(
        "guard",
        field("condition", $.expression),
        "else",
        field("fallback", $.expression),
      ),

    guard_pattern: ($) =>
      seq(
        "guard",
        field("pattern", $.pattern),
        "<-",
        field("value", $.expression),
        "else",
        field("fallback", $.expression),
      ),

    lambda_expression: ($) =>
      prec.right(
        seq(
          field(
            "parameters",
            choice($.identifier, $.lambda_parameter_list),
          ),
          "=>",
          field("body", $.expression),
        ),
      ),

    lambda_parameter_list: ($) =>
      seq(
        "(",
        optional(seq($.lambda_parameter, repeat(seq(",", $.lambda_parameter)))),
        ")",
      ),

    lambda_parameter: ($) =>
      seq(
        field("name", $.identifier),
        optional(seq(":", field("type", $.type))),
      ),

    value_binding: ($) =>
      seq(
        "val",
        field("pattern", $.pattern),
        optional(seq(":", field("type", $.type))),
        "=",
        field("value", $.expression),
      ),

    function_binding: ($) =>
      seq(
        "fun",
        field("name", $.identifier),
        field("parameters", $.parameter_list),
        optional(seq(":", field("return", $.type))),
        "=",
        field("body", $.expression),
      ),

    if_expression: ($) =>
      prec.right(
        seq(
          "if",
          field("condition", $.expression),
          "then",
          field("consequence", $.expression),
          "else",
          field("alternative", $.expression),
        ),
      ),

    case_expression: ($) =>
      prec.right(
        seq(
          "case",
          field("value", $.expression),
          "of",
          $.case_branch,
          repeat(seq("|", $.case_branch)),
        ),
      ),

    case_branch: ($) =>
      seq(
        field("pattern", $.pattern),
        optional(seq("if", field("guard", $.expression))),
        "=>",
        field("body", $.expression),
      ),

    borrow_expression: ($) =>
      prec(5, seq(choice("borrow", "&"), optional("mut"), field("value", $.postfix_expression))),

    binary_expression: ($) =>
      choice(
        ...["|>"].map((operator) =>
          prec.left(
            0,
            seq(
              field("left", $.expression),
              field("operator", operator),
              field("right", $.expression),
            ),
          ),
        ),
        ...["=", "<>", "<", "<=", ">", ">="].map((operator) =>
          prec.left(
            1,
            seq(
              field("left", $.expression),
              field("operator", operator),
              field("right", $.expression),
            ),
          ),
        ),
        ...["::"].map((operator) =>
          prec.right(
            2,
            seq(
              field("left", $.expression),
              field("operator", operator),
              field("right", $.expression),
            ),
          ),
        ),
        ...["+", "-", "^"].map((operator) =>
          prec.left(
            3,
            seq(
              field("left", $.expression),
              field("operator", operator),
              field("right", $.expression),
            ),
          ),
        ),
        ...["*", "/", "%"].map((operator) =>
          prec.left(
            4,
            seq(
              field("left", $.expression),
              field("operator", operator),
              field("right", $.expression),
            ),
          ),
        ),
      ),

    unary_expression: ($) =>
      prec(5, seq("-", field("operand", $.expression))),

    postfix_expression: ($) =>
      prec.left(
        6,
        seq(
          field("value", $.primary_expression),
          repeat(
            choice(
              seq(".", field("field", $.identifier)),
              seq(".", field("index", $.integer)),
            ),
          ),
          optional(field("arguments", $.arguments)),
        ),
      ),

    primary_expression: ($) =>
      choice(
        $.integer,
        $.float,
        $.string,
        $.boolean,
        $.unit_expression,
        $.record_expression,
        $.list_expression,
        $.identifier,
        $.parenthesized_expression,
        $.tuple_expression,
      ),

    unit_expression: ($) => seq("(", ")"),

    parenthesized_expression: ($) =>
      seq("(", field("value", $.expression), ")"),

    tuple_expression: ($) =>
      seq(
        "(",
        field("first", $.expression),
        ",",
        field("second", $.expression),
        repeat(seq(",", $._expression_element)),
        ")",
      ),

    _expression_element: ($) => $.expression,

    record_expression: ($) =>
      seq(
        "{",
        optional(seq($.record_field, repeat(seq(",", $.record_field)))),
        "}",
      ),

    record_field: ($) =>
      seq(
        field("name", $.identifier),
        "=",
        field("value", $.expression),
      ),

    list_expression: ($) =>
      seq(
        "[",
        optional(
          seq(
            $.expression,
            repeat(seq(",", $.expression)),
            optional(","),
          ),
        ),
        "]",
      ),

    arguments: ($) =>
      seq(
        "(",
        optional(seq($.expression, repeat(seq(",", $.expression)))),
        ")",
      ),

    integer: ($) => /\d+/,

    float: ($) => /\d+\.\d+([eE][+-]?\d+)?/,

    string: ($) =>
      token(
        seq(
          '"',
          repeat(choice(/[^"\\]/, /\\./)),
          '"',
        ),
      ),

    boolean: ($) => choice("true", "false"),

    pattern: ($) =>
      choice(
        $.wildcard,
        $.integer,
        $.boolean,
        $.string_pattern,
        $.bytes_pattern,
        $.variable_pattern,
        $.constructor_pattern,
        $.cons_pattern,
        $.tuple_pattern,
        $.list_pattern,
        $.parenthesized_pattern,
        $.record_pattern,
      ),

    cons_pattern: ($) =>
      prec.right(
        2,
        seq(
          field("head", $.pattern),
          "::",
          field("tail", $.pattern),
        ),
      ),

    wildcard: ($) => "_",

    string_pattern: ($) =>
      prec.right(
        2,
        seq(
          field("literal", $.string),
          optional(seq("^", field("rest", $.pattern))),
        ),
      ),

    bytes_pattern: ($) =>
      seq(
        "<<",
        optional(
          seq(
            $._bytes_segment,
            repeat(seq(",", $._bytes_segment)),
            optional(seq(",", field("rest", $.pattern))),
          ),
        ),
        ">>",
      ),

    _bytes_segment: ($) => prec(1, choice($.integer, $.string)),

    variable_pattern: ($) => $.identifier,

    constructor_pattern: ($) =>
      prec(1, seq(
        choice($.constructor, $.qualified_constructor),
        optional(field("argument", $.pattern)),
      )),

    qualified_constructor: ($) =>
      seq(choice($.identifier, $.constructor), ".", $.constructor),

    tuple_pattern: ($) =>
      seq(
        "(",
        field("first", $.pattern),
        ",",
        field("second", $.pattern),
        repeat(seq(",", $._pattern_element)),
        ")",
      ),

    _pattern_element: ($) => $.pattern,

    parenthesized_pattern: ($) =>
      seq("(", field("value", $.pattern), ")"),

    list_pattern: ($) =>
      seq(
        "[",
        optional(
          seq(
            $.pattern,
            repeat(seq(",", $.pattern)),
            optional(","),
          ),
        ),
        "]",
      ),

    record_pattern: ($) =>
      seq(
        "{",
        optional(seq($.record_pattern_field, repeat(seq(",", $.record_pattern_field)))),
        "}",
      ),

    record_pattern_field: ($) =>
      seq(
        field("name", $.identifier),
        "=",
        field("value", $.pattern),
      ),

    identifier: ($) => /[A-Za-z_][A-Za-z0-9_']*/,

    constructor: ($) => token(prec(1, /[A-Z][A-Za-z0-9_']*/)),

    comment: ($) => token(seq("(*", /[^*]*\*+([^*)][^*]*\*+)*/, ")")),
  },
});
