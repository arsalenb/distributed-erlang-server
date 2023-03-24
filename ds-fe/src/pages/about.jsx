import React from "react";
import "./style.css";
import architecture from "../architecture.png";
function About() {
  return (
    <div className="container">
      <p>
        This plateform is meant to provide users with web-based dashboard that
        displays the data collected from the IoT devices of the variety of
        regional servers in a user-friendly way.
      </p>

      <p>
        It is part of a project under a graduate class in University of Pisa:
        "Distributed Systems and Middleware Technologies", and aims to
        demonstrate our understanding of the course material by creating a fully
        functioning application using different notions and technologies
        covered.
      </p>

      <p>
        Erlang was employed as a distributed system that can handle large
        volumes of data incoming from the different regional servers senosors
        and process it in real-time.
      </p>
      <img
        style={{ height: "30em" }}
        alt={"system architecture"}
        src={architecture}
      ></img>
    </div>
  );
}
export default About;
