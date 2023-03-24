import React, { useState, useEffect } from "react";
import useWebSocket from "react-use-websocket";
import { useInterval } from "usehooks-ts";
import _ from "lodash";

export const Context = React.createContext();

const Provider = (props) => {
  const socketUrl = "ws://10.2.1.28:8080/central_server";

  // Appends the last message from the websocket to the its regional server constructed object
  const [rs01Data, setRs01Data] = useState({});
  const [rs02Data, setRs02Data] = useState({});
  const [events, setEvents] = useState([]);

  // Synchronize the change to 1s between each data sent
  const [rs01DataSynchronized, setRs01DataSynchronized] = useState({});
  const [rs02DataSynchronized, setRs02DataSynchronized] = useState({});

  // Averaged readings data per regional server
  const [rs01AveragedData, setRs01AveragedData] = useState({
    temperature: { name: "RS01", data: [{ x: new Date().getTime(), y: 0 }] },
    humidity: { name: "RS01", data: [{ x: new Date().getTime(), y: 0 }] },
  });
  const [rs02AveragedData, setRs02AveragedData] = useState({
    temperature: { name: "RS01", data: [{ x: new Date().getTime(), y: 0 }] },
    humidity: { name: "RS01", data: [{ x: new Date().getTime(), y: 0 }] },
  });

  const { lastMessage } = useWebSocket(socketUrl);

  function averageDataObjectConstructor(
    averagedData,
    regionalServerData,
    serverId
  ) {
    var averagedTemperature = 0;
    var averagedHumidity = 0;

    if (Object.keys(regionalServerData).length !== 0) {
      if (regionalServerData?.temperature) {
        for (let i = 0; i < regionalServerData.temperature.length; i++) {
          averagedTemperature +=
            regionalServerData.temperature[i].data
              .slice(-10)
              .reduce(function (acc, obj) {
                return acc + obj.y;
              }, 0) / regionalServerData.humidity[i].data.slice(-10).length;
        }
        averagedTemperature /= regionalServerData.temperature.length;
      }
      if (regionalServerData?.humidity) {
        for (let i = 0; i < regionalServerData.humidity.length; i++) {
          averagedHumidity +=
            regionalServerData.humidity[i].data
              .slice(-10)
              .reduce(function (acc, obj) {
                return acc + obj.y;
              }, 0) / regionalServerData.humidity[i].data.slice(-10).length;
        }
        averagedHumidity /= regionalServerData.humidity.length;
      }
      // Events system
      if (averagedTemperature > 60) {
        setEvents([
          { type: "error", averagedTemperature, serverId, date: Date() },
          ...events,
        ]);
      } else if (averagedTemperature < 30) {
        setEvents([
          { type: "warning", averagedTemperature, serverId, date: Date() },
          ...events,
        ]);
      }

      return {
        temperature: {
          name: serverId,
          data: [
            ...averagedData.temperature.data,
            { x: new Date().getTime(), y: Math.floor(averagedTemperature) },
          ],
        },
        humidity: {
          name: serverId,
          data: [
            ...averagedData.humidity.data,
            { x: new Date().getTime(), y: Math.floor(averagedHumidity) },
          ],
        },
      };
    }
    return averagedData;
  }

  function objectConstructor(
    regionalServerData,
    { sensor_id, data: dataToParse, data_type, time: timeToParse }
  ) {
    const data = parseInt(dataToParse);
    const time = parseInt(timeToParse);
    if (!regionalServerData.hasOwnProperty(data_type))
      return {
        ...regionalServerData,
        [data_type]: [{ name: sensor_id, data: [{ x: time, y: data }] }],
      };
    else if (
      !regionalServerData[data_type].find((elem) => elem.name === sensor_id)
    ) {
      return {
        ...regionalServerData,
        [data_type]: regionalServerData[data_type].concat({
          name: sensor_id,
          data: [{ x: time, y: data }],
        }),
      };
    } else {
      const dataArray = regionalServerData[data_type].map((elem) => {
        if (elem.name === sensor_id)
          return {
            name: elem.name,
            data: elem.data.concat({ x: time, y: data }),
          };
        else return elem;
      });
      return {
        ...regionalServerData,
        [data_type]: [...dataArray],
      };
    }
  }

  useEffect(() => {
    if (lastMessage !== null) {
      const receivedData = JSON.parse(lastMessage.data);
      if (receivedData["server_id"] === "RS01") {
        setRs01Data(objectConstructor(rs01Data, receivedData));
      } else {
        setRs02Data(objectConstructor(rs02Data, receivedData));
      }
    }
  }, [lastMessage]);

  useInterval(
    () => {
      setRs01DataSynchronized(_.clone(rs01Data));
      setRs02DataSynchronized(_.clone(rs02Data));
    },
    // Delay in milliseconds or null to stop it
    1000
  );

  useInterval(
    () => {
      // Your custom logic here
      setRs01AveragedData(
        averageDataObjectConstructor(rs01AveragedData, rs01Data, "RS01")
      );
      setRs02AveragedData(
        averageDataObjectConstructor(rs02AveragedData, rs02Data, "RS02")
      );
    },
    // Delay in milliseconds or null to stop it
    1000
  );

  return (
    <Context.Provider
      value={{
        rs01DataSynchronized,
        rs02DataSynchronized,
        rs01AveragedData,
        rs02AveragedData,
        events,
      }}
    >
      {props.children}
    </Context.Provider>
  );
};

export default Provider;
