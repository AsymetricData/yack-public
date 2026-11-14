; Keywords and literals
[
  "fun"
  "val"
  "let"
  "arena"
  "in"
  "end"
  "if"
  "then"
  "else"
  "case"
  "of"
  "guard"
  "borrow"
  "mut"
  "from"
  "consume"
  "type"
  "datatype"
  "import"
  "as"
] @keyword

"&" @operator

(boolean) @constant.builtin.boolean
(integer) @constant.numeric.integer
(float) @constant.numeric.float
(string) @string
(wildcard) @variable.builtin

; Types
(primitive_type) @type.builtin
(type_identifier) @type
(type_variable) @type
(type_declaration name: (identifier) @type)
(datatype_declaration name: (identifier) @type)
(qualified_type (identifier) @namespace)
(qualified_type (constructor) @type)

; Constructors
(constructor) @constructor
(constructor_declaration name: (constructor) @constructor)
(qualified_constructor (identifier) @namespace)
(qualified_constructor (constructor) @constructor)

; Modules
(module_path) @namespace
(import_item name: (identifier) @variable)

; Functions and parameters
(function_declaration name: (identifier) @function)
(function_binding name: (identifier) @function)
(parameter pattern: (pattern (variable_pattern (identifier) @variable.parameter)))
(lambda_parameter name: (identifier) @variable.parameter)
((postfix_expression
  value: (primary_expression (identifier) @constructor)
  arguments: (arguments))
 (#match? @constructor "^[A-Z]"))

((postfix_expression
  value: (primary_expression (identifier) @function.call)
  arguments: (arguments))
 (#match? @function.call "^[a-z_]"))

; Bindings and fields
(variable_pattern (identifier) @variable)
(record_field name: (identifier) @property)
(field_declaration name: (identifier) @property)
(record_pattern_field name: (identifier) @property)

; Operators and punctuation
(binary_expression operator: _ @operator)
(cons_pattern "::" @operator)
(string_pattern "^" @operator)
(bytes_pattern ["<<" ">>"] @punctuation.bracket)
(unary_expression "-" @operator)
(function_type "->" @operator)
(lambda_expression "=>" @operator)
(guard_pattern "<-" @operator)

["(" ")" "{" "}" "[" "]"] @punctuation.bracket
["," ":" "."] @punctuation.delimiter

; Comments
(comment) @comment
