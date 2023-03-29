import React, { useContext } from "react";
import { Space, Card } from "antd";
import ChartComponent from "../components/ChartComponent";
import { Context } from "../context/context";

function Temperature() {
  const { rs01DataSynchronized, rs02DataSynchronized } = useContext(Context);

  var rs01TemperatureData = [{ name: "Initialising", data: [{ x: 0, y: 0 }] }];
  var rs02TemperatureData = [{ name: "Initialising", data: [{ x: 0, y: 0 }] }];

  if (rs01DataSynchronized?.temperature)
    rs01TemperatureData = rs01DataSynchronized.temperature;
  if (rs02DataSynchronized?.temperature)
    rs02TemperatureData = rs02DataSynchronized.temperature;

  return (
    <Space className="spacer" size={"large"}>
      <Card
        title="Regional Server RS01"
        style={{ width: "100%", height: "90%" }}
      >
        <ChartComponent
          data={rs01TemperatureData}
          colors={["#eb3d34", "#e8eb34", "#3440eb"]}
          xAxisLabel="Time (Minutes/Seconds)"
          yAxisLabel="Temperature (Celsius)"
        />
      </Card>
      <Card
        title="Regional Server RS02"
        style={{ width: "100%", height: "90%" }}
      >
        <ChartComponent
          data={rs02TemperatureData}
          colors={["#993955", "#BDC667", "#E9ECF5"]}
          xAxisLabel="Time (Minutes/Seconds)"
          yAxisLabel="Temperature (Celsius)"
        />
      </Card>
    </Space>
  );
}
export default Temperature;
