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


def send_rand_data_tx(SERVER_URL='http://localhost:8080', LOW=10, HIGH=30, Reps=10, Delay=3):
    for i in range(Reps):
        timestamp = datetime.now().isoformat(timespec='seconds') 
        random_data1 = random.randint(LOW, HIGH)
        string_to_send = 'msg_type=data_tx&sensor_id=001&sensor_data='+str(random_data1)+'&time=' + str(timestamp)
        r = requests.post(SERVER_URL, data = string_to_send)
        sleep(random.randint(1, Delay))
        
        timestamp = datetime.now().isoformat(timespec='seconds') 
        random_data2 = random.randint(LOW, HIGH)
        string_to_send = 'msg_type=data_tx&sensor_id=002&sensor_data='+str(random_data2)+'&time=' + str(timestamp)
        r = requests.post(SERVER_URL, data = string_to_send)
        sleep(random.randint(1, Delay))
        print('Sensor 001: ', random_data1, ' / Sensor 002: ', random_data2)

def send_data_tx(SERVER_URL='http://localhost:8080', SensorID='001', Data=20):
    timestamp = datetime.now().isoformat(timespec='seconds') 
    string_to_send = 'msg_type=data_tx&sensor_id='+SensorID+'&sensor_data='+str(Data)+'&time='+str(timestamp)
    r = requests.post(SERVER_URL, data = string_to_send)
    print(r.text)
    
def send_event(SERVER_URL='http://localhost:8080', ServerID='RS002', EventType='UPPER_TS'):
    timestamp = datetime.now().isoformat(timespec='seconds') 
    string_to_send = 'msg_type=event&event_info='+ServerID+' Warning! '+EventType+' Crossed at: '+str(timestamp)+' / Avg: 12.30 C - Received From Sensor: 003 - Reading: 33 C'
    r = requests.post(SERVER_URL, data = string_to_send)
    print(r.text)
    
#string_to_send = 'server_id=SS'+str(random_id)+'&threshold=HIGH TEMP&value='+str(random_data+10)+'&timestamp=' + str(timestamp) 
#print(string_to_send, '\n sending SERVER POST to server:')

#r = requests.post(SERVER_URL, data = string_to_send)
#print(r.status_code, ' Server Reply: ', r.text, '\n\n Sending GET to Server:')

#r = requests.get(SERVER_URL)
#print('GET REQUEST:\n', r.status_code, ' Server Reply: ', r.text)

#print(f"Status Code: {r.status_code}, Response: {r.json()}")
send_rand_data_tx()

