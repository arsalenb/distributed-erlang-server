{application, 'rest_hello_world', [
	{description, "Cowboy REST Hello World example"},
	{vsn, "1"},
	{modules, ['rest_hello_world_app','rest_hello_world_sup','toppage_h']},
	{registered, [rest_hello_world_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {rest_hello_world_app, []}},
	{env, []}
]}.