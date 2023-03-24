import React, { useContext } from "react";
import { Alert } from "antd";
import { Context } from "../context/context";

function Events() {
  const { events } = useContext(Context);
  return (
    <div>
      {events.map((e) => {
        if (e.type === "error") {
          return (
            <Alert
              style={{ marginTop: "2em" }}
              message={`${e.serverId} - ${e.date}`}
              description="Upper Temperature Average Threshold Crossed, Immediate Attention Required! "
              type="error"
              showIcon
            />
          );
        } else {
          return (
            <Alert
              style={{ marginTop: "1em" }}
              message={`${e.serverId} - ${e.date}`}
              description="Lower Temperature Average Threshold Crossed..."
              type="warning"
              showIcon
              closable
            />
          );
        }
      })}
    </div>
  );
}
export default Events;
