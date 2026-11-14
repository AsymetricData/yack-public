; Scopes
(function_declaration) @local.scope
(function_binding) @local.scope
(let_expression) @local.scope
(arena_expression) @local.scope
(lambda_expression) @local.scope
(case_branch) @local.scope

; Definitions
(function_declaration name: (identifier) @local.definition)
(function_binding name: (identifier) @local.definition)
(parameter pattern: (pattern (variable_pattern) @local.definition))
(lambda_parameter name: (identifier) @local.definition)
(variable_pattern) @local.definition

; References
(identifier) @local.reference
