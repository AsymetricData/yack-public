(function_declaration) @function.outer
(function_declaration body: (expression) @function.inner)
(function_binding) @function.outer
(function_binding body: (expression) @function.inner)

(type_declaration) @class.outer
(type_declaration body: (type (record_type) @class.inner))
(datatype_declaration) @class.outer

(let_expression) @block.outer
(if_expression) @block.outer
(case_expression) @block.outer
(case_branch) @block.inner

(parameter) @parameter.outer
(parameter pattern: (pattern (variable_pattern) @parameter.inner))

(comment) @comment.outer
