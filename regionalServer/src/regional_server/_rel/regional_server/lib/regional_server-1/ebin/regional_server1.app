{application, 'regional_server1', [
	{description, "Cowboy REST Hello World example"},
	{vsn, "1"},
	{modules, ['average_calc_task','data_log_task','event_handler_task','msg_formatting','regional_server_app','regional_server_sup','toppage_h']},
	{registered, []},
	{applications, [kernel,stdlib,cowboy]},
	{env, []}
]}.