import React, { useContext } from "react";
import { Space, Card } from "antd";
import ChartComponent from "../components/ChartComponent";
import { Context } from "../context/context";

function Humidity() {
  const { rs01DataSynchronized, rs02DataSynchronized } = useContext(Context);

  var rs01HumidityData = [{ name: "Initialising", data: [{ x: 0, y: 0 }] }];
  var rs02HumidityData = [{ name: "Initialising", data: [{ x: 0, y: 0 }] }];

  if (rs01DataSynchronized?.humidity)
    rs01HumidityData = rs01DataSynchronized.humidity;
  if (rs02DataSynchronized?.humidity)
    rs02HumidityData = rs02DataSynchronized.humidity;

  return (
    <Space className="spacer" size={"large"}>
      <Card
        title="Regional Server RS01"
        style={{ width: "100%", height: "90%" }}
      >
        <ChartComponent
          data={rs01HumidityData}
          xAxisLabel="Time (Minutes/Seconds)"
          yAxisLabel="Humidity (%)"
        />
      </Card>
      <Card
        title="Regional Server RS02"
        style={{ width: "100%", height: "90%" }}
      >
        <ChartComponent
          data={rs02HumidityData}
          colors={["#EEE3AB", "#A77E58", "#BA3F1D"]}
          xAxisLabel="Time (Minutes/Seconds)"
          yAxisLabel="Humidity (%)"
        />
      </Card>
    </Space>
  );
}
export default Humidity;
