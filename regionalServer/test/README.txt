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
    -> configure CENTRAL_SERVER_NODE in the toppage code
    -> create a test node:
    -> erl -sname central_server@localhost -setcookie 'monitoring_cookie'
    -> the server is configured to send an event message to this node when a threshold cross happens
    -> setup a listening function:
    -> register (data_comm, self()).
    -> receive {{ID, Data, DataType}, From} -> ID,Data,DataType end.
    
