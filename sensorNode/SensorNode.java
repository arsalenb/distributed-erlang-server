package sensorNode;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.time.Instant;
import java.util.Random;

public class SensorNode {
    private static final String REGIONAL_SERVER_URL = "http://10.2.1.27:8080/regional_server";
    private static final int SEND_INTERVAL = 1000;
    public static void main(String[] args) {
        while (true) {
            try {

                sendSensorData("HS01", "humidity");
                sendSensorData("TS01", "temperature");
                Thread.sleep(SEND_INTERVAL);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private static void sendSensorData(String sensorId, String sensorDataType) throws Exception {
        String sensorData = generateSensorData(sensorId, sensorDataType);
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


    private static String generateSensorData(String sensorId, String sensorDataType) {
        Random random = new Random();
        int sensorData;

        if (sensorDataType.equals("temperature")) {
            sensorData = random.nextInt(21) + 10;
        } else {
            sensorData = random.nextInt(15) + 85;
        }

        long currentTime = Instant.now().toEpochMilli();
        return String.format("sensor_id=%s&sensor_data=%d&sensor_data_type=%s&time=%d",
                sensorId, sensorData, sensorDataType, currentTime);
    }

}

