from pandas import read_csv
from datetime import datetime
import requests
import random

### FOR TESTING ONLY ###

SERVER_URL = 'http://localhost:8080'


random_id = random.randint(0, 2000)
print("ID ", random_id)
random_data = round(random_id / random.randint(0, 200), 2)
print("Data ", random_data)
timestamp = datetime.now()
print("Date and Time: ", timestamp)

## toppage_h.erl -> erlang proplist:get_value function is used to get data from the POST message
## toppage_h.erl -> the list is retrieved from cowboy request handler function cowboy_req:read_urlencoded_body
## cowboy_req.erl -> cowboy request handler reads the binary values from the message using cow_qs:parse_qs(Body)
## cow_qs.erl -> the parse function reads the characters from the binary form and copies to an erlang list
## the parse function uses the '&' to seaparate key value pairs in the string
## the data string format to be sent in the POST request is then: key1=value1&key2=value2&key3=value3...
string_to_send = 'sensor_id='+str(random_id)+'&sensor_data='+str(random_data)+'&timestamp=' + str(timestamp) 
print(string_to_send, '\n sending POST to server:')

r = requests.post(SERVER_URL, data = string_to_send)
print('Server Reply: ', r.text)
#print(f"Status Code: {r.status_code}, Response: {r.json()}")


