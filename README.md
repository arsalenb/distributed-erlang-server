# 879II - Distributed Systems and Middleware Technologies: Weather Monitoring using Erlang

This project was developed for the **Distributed Systems and Middleware Technologies** course. It implements a **distributed monitoring platform** for remote evaluation of weather parameters using a combination of **Erlang servers** and a **Tomcat-based web application**.

The goal of the platform is to provide a **flexible and expandable monitoring system**, displaying real-time data from multiple remote regions, as well as an **event and warning interface**.

---

## Project Description

In the developed application:

- **Emulated sensors** periodically generate temperature and humidity data.  
- Sensor data is transmitted to **Regional Servers** via an exposed **REST interface**.  
- Regional Servers use **Erlang’s message-passing system** to forward telemetry data to a **Central Server**.  
- The **Central Server** streams data to a **Tomcat application server** using **WebSockets**.  
- The Tomcat server hosts a **React.js frontend** that collects and displays real-time charts and processes readings to generate **events and warnings** based on predefined thresholds.

### Components
- **Distributed Erlang servers** – Regional and Central Servers handling sensor data and message passing.  
- **REST interfaces** – Receive telemetry data from emulated sensors.  
- **Tomcat-based web application** – Displays real-time charts and event/warning notifications using WebSockets.  
- **Emulated sensors** – Generate test data to simulate remote regions.
---

## Deployment & Execution

Deployment instructions and detailed steps for running the platform can be found in the **`how_to_run.txt`** file in the repository.

---
