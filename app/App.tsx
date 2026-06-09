import React from "react";
import { StatusBar } from "expo-status-bar";
import { NavigationContainer } from "@react-navigation/native";
import { createNativeStackNavigator } from "@react-navigation/native-stack";
import { SafeAreaProvider } from "react-native-safe-area-context";
import { ComposeScreen } from "./src/screens/ComposeScreen";
import { ResultScreen } from "./src/screens/ResultScreen";
import { HistoryScreen } from "./src/screens/HistoryScreen";
import { HistoryItem } from "./src/types";

export type RootStackParamList = {
  Compose: undefined;
  Result: { item: HistoryItem };
  History: undefined;
};

const Stack = createNativeStackNavigator<RootStackParamList>();

export default function App() {
  return (
    <SafeAreaProvider>
      <NavigationContainer>
        <Stack.Navigator>
          <Stack.Screen
            name="Compose"
            component={ComposeScreen}
            options={{ title: "Business Email" }}
          />
          <Stack.Screen name="Result" component={ResultScreen} options={{ title: "Email" }} />
          <Stack.Screen name="History" component={HistoryScreen} options={{ title: "History" }} />
        </Stack.Navigator>
        <StatusBar style="auto" />
      </NavigationContainer>
    </SafeAreaProvider>
  );
}
