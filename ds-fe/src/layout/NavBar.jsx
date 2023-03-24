import React, { useState, useEffect } from "react";
import { Layout, Menu } from "antd";
import "./style.css";
import logo from "../logo.png"; // relative path to image
import { useLocation, useNavigate } from "react-router-dom";

function NavBar({ items }) {
  const location = useLocation();
  const navigate = useNavigate();
  const [selectedKey, setSelectedKey] = useState(
    items.find((_item) => location.pathname.startsWith(_item.key))?.key
  );
  function onClick(item) {
    const clicked = items.find((_item) => _item.key === item.key);
    navigate(clicked.key);
  }
  useEffect(() => {
    setSelectedKey(items.find((_item) => location.pathname === _item.key)?.key);
  }, [location, items]);

  return (
    <Layout.Header>
      <div className="logo">
        <img alt={"logo"} src={logo} style={{ height: "5em" }}></img>
      </div>

      <Menu
        theme="dark"
        mode="horizontal"
        selectedKeys={[selectedKey]}
        style={{ justifyContent: "flex-end" }}
        items={items}
        onClick={onClick}
      />
    </Layout.Header>
  );
}
export default NavBar;
