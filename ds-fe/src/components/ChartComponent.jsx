import React from "react";
import Chart from "react-apexcharts";

function ChartComponent({ colors, data, xAxisLabel, yAxisLabel }) {
  const options = {
    chart: {
      id: "realtime",
      animations: {
        enabled: true,
        easing: "linear",
        dynamicAnimation: {
          speed: 1000,
        },
      },
      toolbar: {
        show: false,
      },
      zoom: {
        enabled: false,
      },
    },
    dataLabels: {
      enabled: false,
    },
    stroke: {
      curve: "smooth",
    },

    markers: {
      size: 0,
    },
    xaxis: {
      type: "datetime",
      range: 10000,
      labels: {
        format: "mm/ss",
      },

      title: {
        text: xAxisLabel,
      },
    },
    yaxis: {
      title: {
        text: yAxisLabel,
      },
      range: 10,
    },
    legend: {
      position: "top",
      horizontalAlign: "right",
      floating: true,
      offsetY: -10,
      offsetX: -0,
    },
    tooltip: {
      x: {
        format: "mm/ss",
      },
    },
    colors: colors,
  };
  return (
    <Chart
      options={options}
      series={data}
      type="line"
      height="90%"
      width="100%"
    />
  );
}
export default ChartComponent;
