import React from "react";
import { Routes, Route } from "react-router-dom";
import NoMatch from "./layout/NoMatch";
import AppLayout from "./layout/AppLayout";
import About from "./pages/about";
import Temperature from "./pages/Temperature";
import Events from "./pages/Events";
import Provider from "./context/context";
import Dashboard from "./pages/Dashboard";
import Humidity from "./pages/Humidity";

function App() {
  return (
    <Provider>
      <Routes>
        <Route path="/monitoring" element={<AppLayout />}>
          <Route index element={<Dashboard />} />
          <Route path="/monitoring/temperature" element={<Temperature />} />
          <Route path="/monitoring/humidity" element={<Humidity />} />
          <Route path="/monitoring/events" element={<Events />} />
          <Route path="/monitoring/about" element={<About />} />
          <Route path="*" element={<NoMatch />} />
        </Route>
      </Routes>
    </Provider>
  );
}

export default App;
