import React from "react";
import { Layout, Breadcrumb } from "antd";
import "./style.css";
import { Outlet } from "react-router-dom";
import { useLocation } from "react-router-dom";

function Content({ items }) {
  const location = useLocation();
  return (
    <Layout.Content style={{ padding: "0 50px", margin: "16px 0" }}>
      <Breadcrumb style={{ margin: "16px 0" }}>
        <Breadcrumb.Item>Home</Breadcrumb.Item>
        <Breadcrumb.Item>
          {items.find((_item) => location.pathname === _item.key)?.label}
        </Breadcrumb.Item>
      </Breadcrumb>
      <div className="site-layout-content" style={{ background: "white" }}>
        <Outlet />
      </div>
    </Layout.Content>
  );
}
export default Content;
