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

string_to_send = 'timestamp=' + str(timestamp)
print(string_to_send, '\n sending POST to server:')

r = requests.post(SERVER_URL, data = string_to_send)
print(r.text)
#print(f"Status Code: {r.status_code}, Response: {r.json()}")


