import React from "react";

import NavBar from "./NavBar";
import MainContent from "./Content";
import Footer from "./Footer";
import { Layout } from "antd";

import "./style.css";

const items = [
  { key: "/monitoring/", label: "Dashboard" },
  { key: "/monitoring/temperature", label: "Temperature" },
  { key: "/monitoring/humidity", label: "Humidity" },
  { key: "/monitoring/events", label: "Events" },
  { key: "/monitoring/about", label: "About" },
];

function AppLayout() {
  return (
    <Layout className="layout">
      <NavBar items={items} />
      <MainContent items={items} />
      <Footer />
    </Layout>
  );
}
export default AppLayout;
