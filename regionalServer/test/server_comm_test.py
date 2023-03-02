from pandas import read_csv
from datetime import datetime
import requests
import random
from time import sleep

### FOR TESTING ONLY ###

## Message formating to interact with cowboy message handler:
## toppage_h.erl -> erlang proplist:get_value function is used to get data from the POST message
## toppage_h.erl -> the list is retrieved from cowboy request handler function cowboy_req:read_urlencoded_body
## cowboy_req.erl -> cowboy request handler reads the binary values from the message using cow_qs:parse_qs(Body)
## cow_qs.erl -> the parse function reads the characters from the binary form and copies to an erlang list
## the parse function uses the '&' to seaparate key value pairs in the string
## the data string format to be sent in the POST request is then: key1=value1&key2=value2&key3=value3...
## example_string = 'msg_type=data_tx&sensor_id=001&sensor_data=20&time=2022/01/02T00:01:20

def send_rand_data(SERVER_URL='http://localhost:8080', LOW=10, HIGH=30, Reps=10, Delay=3):
    for i in range(Reps):
        timestamp = datetime.now().isoformat(timespec='seconds') 
        random_data1 = random.randint(LOW, HIGH)
        string_to_send = 'msg_type=data_tx&sensor_id=AAA&sensor_data='+str(random_data1)+'&time=' + str(timestamp)
        r = requests.post(SERVER_URL, data = string_to_send)
        sleep(random.randint(1, Delay))
        
        timestamp = datetime.now().isoformat(timespec='seconds') 
        random_data2 = random.randint(LOW, HIGH)
        string_to_send = 'msg_type=data_tx&sensor_id=BBB&sensor_data='+str(random_data2)+'&time=' + str(timestamp)
        r = requests.post(SERVER_URL, data = string_to_send)
        sleep(random.randint(1, Delay))
        print('Sensor AAA: ', random_data1, ' / Sensor BBB: ', random_data2)

def send_data(SERVER_URL='http://localhost:8080', SensorID='AAA', Data=20):
    timestamp = datetime.now().isoformat(timespec='seconds') 
    string_to_send = 'msg_type=data_tx&sensor_id='+SensorID+'&sensor_data='+str(Data)+'&time='+str(timestamp)
    r = requests.post(SERVER_URL, data = string_to_send)
    print(r.text)
    
    
send_rand_data_tx()

