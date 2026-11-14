(function_declaration name: (identifier) @definition.function)
(function_binding name: (identifier) @definition.function)
(type_declaration name: (identifier) @definition.type)
(datatype_declaration name: (identifier) @definition.type)
(constructor_declaration name: (constructor) @definition.constructor)

(postfix_expression
  value: (primary_expression (identifier) @reference.call)
  arguments: (arguments))

(type_identifier) @reference.type
(constructor) @reference.constructor
