package centralServer.erl_interface;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.time.Instant;
import java.util.Random;

public class SensorNode {
    private static final String REGIONAL_SERVER_URL = "http://127.0.0.1:8080/regional_server"; // http://10.2.1.28:8080/regional_server
    private static final int SEND_INTERVAL = 5000;

    public static void main(String[] args) {
        while (true) {
            try {
                sendSensorData();
                Thread.sleep(SEND_INTERVAL);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private static void sendSensorData() throws Exception {
        String sensorData = generateSensorData();
        System.out.println("Generated sensor data: " + sensorData);
        HttpURLConnection connection = null;

        try {
            URL url = new URL(REGIONAL_SERVER_URL);
            connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("POST");
            connection.setDoOutput(true);

            OutputStream os = connection.getOutputStream();
            os.write(sensorData.getBytes());
            os.flush();
            os.close();

            int responseCode = connection.getResponseCode();
            System.out.println("Response code: " + responseCode);

            BufferedReader br = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            String line;
            StringBuilder response = new StringBuilder();

            while ((line = br.readLine()) != null) {
                response.append(line);
            }
            br.close();

            System.out.println("Response: " + response.toString());
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    private static String generateSensorData() {
        Random random = new Random();
        String sensorId;
        String sensorDataType;
        int sensorData;

        if (random.nextBoolean()) {
            sensorId = "TS01";
            sensorDataType = "temperature";
            sensorData = random.nextInt(21) + 10;
        } else {
            sensorId = "HS01";
            sensorDataType = "humidity";
            sensorData = random.nextInt(15) + 85;
        }

        long currentTime = Instant.now().toEpochMilli();
        return String.format("sensor_id=%s&sensor_data=%d&sensor_data_type=%s&time=%d",
                sensorId, sensorData, sensorDataType, currentTime);
    }
}

