erl -sname erl_comm_interface@central -setcookie 'monitoring_cookie'
c(erl_interface).
erl_interface:start_listener(self()).
