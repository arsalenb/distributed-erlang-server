Regional Monitoring Server Testing

- To run the demo server:
    -> go to 'src/regional_server'
    -> in the shell use <make run>
    
- Check static HTML page: 
    -> in the browser go to 'http://localhost:8080/regional_server'
	-> when deployed 'http://10.2.1.27:8080/regional_server' and 'http://10.2.1.34:8080/regional_server'
    
- Test sending POST messages - Sensor Data
    -> use python functions (run python3)
    -> import server_comm_test
    -> server_comm_test.send_rand_data()
    -> server_comm_test.send_data()
    
    
