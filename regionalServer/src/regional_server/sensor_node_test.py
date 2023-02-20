from pandas import read_csv
from datetime import datetime
import requests
import random
from time import sleep

### FOR TESTING ONLY ###

SERVER_URL = 'http://localhost:8080'


## toppage_h.erl -> erlang proplist:get_value function is used to get data from the POST message
## toppage_h.erl -> the list is retrieved from cowboy request handler function cowboy_req:read_urlencoded_body
## cowboy_req.erl -> cowboy request handler reads the binary values from the message using cow_qs:parse_qs(Body)
## cow_qs.erl -> the parse function reads the characters from the binary form and copies to an erlang list
## the parse function uses the '&' to seaparate key value pairs in the string
## the data string format to be sent in the POST request is then: key1=value1&key2=value2&key3=value3...
#example_to_send = 'msg_type=data_tx&sensor_id=001&sensor_data='+str(random_data)+'&time=' + str(timestamp)

for i in range(10):
    timestamp = datetime.now().isoformat(timespec='seconds') 
    random_data1 = random.randint(10, 30)
    string_to_send = 'msg_type=data_tx&sensor_id=001&sensor_data='+str(random_data1)+'&time=' + str(timestamp)
    r = requests.post(SERVER_URL, data = string_to_send)
    sleep(random.randint(1, 3))
    
    timestamp = datetime.now().isoformat(timespec='seconds') 
    random_data2 = random.randint(10, 30)
    string_to_send = 'msg_type=data_tx&sensor_id=002&sensor_data='+str(random_data2)+'&time=' + str(timestamp)
    r = requests.post(SERVER_URL, data = string_to_send)
    sleep(random.randint(1, 3))
    print(random_data1, '    ', random_data2)
    #print(string_to_send1, '\n POST REQUEST:\n', r1.status_code, ' Server Reply: ', r1.text, '\n\n')
    #print(string_to_send2, '\n POST REQUEST:\n', r2.status_code, ' Server Reply: ', r2.text, '\n\n')
timestamp = datetime.now().isoformat(timespec='seconds') 
string_to_send = 'msg_type=data_tx&sensor_id=001&sensor_data=50&time=' + str(timestamp) # High Data value
r = requests.post(SERVER_URL, data = string_to_send)

#string_to_send = 'server_id=SS'+str(random_id)+'&threshold=HIGH TEMP&value='+str(random_data+10)+'&timestamp=' + str(timestamp) 
#print(string_to_send, '\n sending SERVER POST to server:')

#r = requests.post(SERVER_URL, data = string_to_send)
#print(r.status_code, ' Server Reply: ', r.text, '\n\n Sending GET to Server:')

#r = requests.get(SERVER_URL)
#print('GET REQUEST:\n', r.status_code, ' Server Reply: ', r.text)

#print(f"Status Code: {r.status_code}, Response: {r.json()}")


