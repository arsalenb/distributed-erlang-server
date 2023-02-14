{application, 'regional_server', [
	{description, "Cowboy REST Hello World example"},
	{vsn, "1"},
	{modules, ['regional_server_app','regional_server_sup','toppage_h']},
	{registered, [regional_server_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {regional_server_app, []}},
	{env, []}
]}.