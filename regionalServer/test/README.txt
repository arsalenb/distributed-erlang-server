Regional Monitoring Server Testing

- To run the demo server:
    -> go to 'src/regional_server'
    -> in the shell use <make run>
    
- Check static HTML page: 
    -> in the browser go to 'http://localhost:8080/'
    
- Test sending POST messages - Sensor Data
    -> use python functions (run python3)
    -> import server_comm_test
    -> server_comm_test.send_rand_data()
    -> server_comm_test.send_data()
    
- Test Node connection:
    -> create a test node:
    -> erl -sname regional_serverN@localhost -setcookie 'monitoring_cookie'
    -> the server is configured to send an event message to this node when a threshold cross happens
    -> setup a listening function:
    -> register (event_comm, self()).
    -> receive {Event, From} -> Event end.
    
    -> create a second test node:
    -> erl -sname regional_serverK@localhost -setcookie 'monitoring_cookie'
    -> send an event message to the regional_server:
    -> {event_comm, regional_server@localhost} ! {["TESTING EVENT TRANSMISSION"], self()}.
    
    -> check envent list at 'http://localhost:8080/'
