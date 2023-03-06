import requests
import random
import time
from time import sleep

SERVER1_URL='http://10.2.1.27:8080/regional_server'
SERVER2_URL='http://10.2.1.34:8080/regional_server'


def sensor_send(url=SERVER1_URL, s_id='TS0N', d_type='temperature'):
    t = time.time()
    timestamp = int(t)
    if d_type == 'temperature':
        reading = random.randint(10, 30)
    elif d_type == 'humidity':
        reading = random.randint(85, 99)
    string_to_send = 'sensor_id='+s_id+'&sensor_data='+str(reading)+'&sensor_data_type='+d_type+'&time='+str(timestamp)
    r = requests.post(url, data = string_to_send)
    print("Sensor: ",s_id," Sent Reading: ", str(reading))
    print("Regional Server Reply: ", r.text)
    
def ts01_send():
    sensor_send(SERVER1_URL, 'TS01', 'temperature')
    
def hs01_send():
    sensor_send(SERVER1_URL, 'HS01', 'humidity')

def ts02_send():
    sensor_send(SERVER2_URL, 'TS02', 'temperature')

def hs02_send():
    sensor_send(SERVER2_URL, 'HS02', 'humidity')

def run_test(mode = 1, runs = 600):
    if mode == 1:
        for i in range (runs):
            ts01_send()
            sleep(0.5)
            hs01_send()
            sleep(0.5)
    elif mode == 2:
        for i in range (runs):
            ts01_send()
            sleep(0.25)
            ts02_send()
            sleep(0.25)
            hs01_send()
            sleep(0.25)
            hs02_send()
            sleep(0.25)
    
run_test()

