import React, { useState, useEffect, useContext } from "react";
import { Space, Card } from "antd";
import ChartComponent from "../components/ChartComponent";
import "./style.css";
import { Context } from "../context/context";

import Events from "./Events";
function Dashboard() {
  const { rs01AveragedData } = useContext(Context);
  const [data, setData] = useState([
    {
      x: new Date().getTime(),
      y: 0,
    },
  ]);
  const [data1, setData1] = useState([
    {
      x: new Date().getTime(),
      y: 0,
    },
  ]);

  function getRandomArbitrary(min, max) {
    min = Math.ceil(min);
    max = Math.floor(max);
    return Math.floor(Math.random() * (max - min) + min);
  }

  useEffect(() => {
    if (data.length > 200) setData(data.slice(-50));
    const interval = setInterval(() => {
      setData([
        ...data,
        {
          x: new Date().getTime(),

          y: getRandomArbitrary(1, 100),
        },
      ]);
    }, 1000);

    return () => clearInterval(interval);
  }, [data]);
  useEffect(() => {
    if (data1.length > 200) setData(data1.slice(-50));
    const interval = setInterval(() => {
      setData1([
        ...data1,
        {
          x: new Date().getTime(),

          y: getRandomArbitrary(50, 80),
        },
      ]);
    }, 1000);

    return () => clearInterval(interval);
  }, [data1]);

  return (
    <>
      {" "}
      <Space
        size={"large"}
        style={{
          height: " calc(70vh - 19em)",
          width: "-webkit-fill-available",
        }}
      >
        <Card
          title="Central Average Temperature"
          style={{ width: "100%", height: "90%" }}
        >
          <ChartComponent
            data={[rs01AveragedData.temperature]}
            colors={["#eb3d34", "#e8eb34", "#3440eb"]}
            xAxisLabel="Time (Minutes/Seconds)"
            yAxisLabel="Temperature (Celsius)"
          />
        </Card>
        <Card
          title="Central Average Humidity"
          style={{ width: "100%", height: "90%" }}
        >
          <ChartComponent
            data={[rs01AveragedData.humidity]}
            xAxisLabel="Time (Minutes/Seconds)"
            yAxisLabel="Humidity (%)"
          />
        </Card>
      </Space>
      <Card
        title="Central Critical Events"
        style={{ width: "100%", height: "20em", overflow: "auto" }}
      >
        <Events />
      </Card>
    </>
  );
}
export default Dashboard;
