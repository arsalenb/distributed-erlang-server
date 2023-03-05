erl -sname middle_node@testnode -setcookie 'monitoring_cookie'
c(erl_interface).
erl_interface:start_listener(self()).
