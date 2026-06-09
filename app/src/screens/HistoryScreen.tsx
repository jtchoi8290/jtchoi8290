import React, { useCallback, useState } from "react";
import { FlatList, Pressable, StyleSheet, Text, View, Alert } from "react-native";
import { useFocusEffect } from "@react-navigation/native";
import { NativeStackScreenProps } from "@react-navigation/native-stack";
import { clearHistory, loadHistory } from "../storage/history";
import { HistoryItem } from "../types";
import { RootStackParamList } from "../../App";

type Props = NativeStackScreenProps<RootStackParamList, "History">;

export function HistoryScreen({ navigation }: Props) {
  const [items, setItems] = useState<HistoryItem[]>([]);

  useFocusEffect(
    useCallback(() => {
      loadHistory().then(setItems);
    }, []),
  );

  const onClear = () => {
    Alert.alert("Clear history?", "This cannot be undone.", [
      { text: "Cancel", style: "cancel" },
      {
        text: "Clear",
        style: "destructive",
        onPress: async () => {
          await clearHistory();
          setItems([]);
        },
      },
    ]);
  };

  if (items.length === 0) {
    return (
      <View style={styles.empty}>
        <Text style={styles.emptyText}>No history yet.</Text>
      </View>
    );
  }

  return (
    <View style={{ flex: 1, backgroundColor: "#f9fafb" }}>
      <FlatList
        data={items}
        keyExtractor={(item) => item.id}
        contentContainerStyle={{ padding: 16 }}
        renderItem={({ item }) => (
          <Pressable
            style={styles.row}
            onPress={() => navigation.navigate("Result", { item })}
          >
            <Text style={styles.rowLang}>
              {item.request.language.toUpperCase()} · {item.request.tone} · {item.request.recipient}
            </Text>
            <Text style={styles.rowSubject} numberOfLines={1}>
              {item.subject}
            </Text>
            <Text style={styles.rowBody} numberOfLines={2}>
              {item.body}
            </Text>
            <Text style={styles.rowDate}>{new Date(item.createdAt).toLocaleString()}</Text>
          </Pressable>
        )}
      />
      <Pressable style={styles.clear} onPress={onClear}>
        <Text style={styles.clearText}>Clear All</Text>
      </Pressable>
    </View>
  );
}

const styles = StyleSheet.create({
  empty: { flex: 1, alignItems: "center", justifyContent: "center", backgroundColor: "#f9fafb" },
  emptyText: { color: "#6b7280", fontSize: 15 },
  row: {
    backgroundColor: "#fff",
    borderRadius: 12,
    padding: 14,
    marginBottom: 10,
    borderWidth: 1,
    borderColor: "#e5e7eb",
  },
  rowLang: { fontSize: 11, color: "#6b7280", textTransform: "uppercase", marginBottom: 4, fontWeight: "600" },
  rowSubject: { fontSize: 15, fontWeight: "600", color: "#111827", marginBottom: 4 },
  rowBody: { fontSize: 13, color: "#4b5563", lineHeight: 18 },
  rowDate: { fontSize: 11, color: "#9ca3af", marginTop: 6 },
  clear: { padding: 16, alignItems: "center", borderTopWidth: 1, borderTopColor: "#e5e7eb", backgroundColor: "#fff" },
  clearText: { color: "#dc2626", fontWeight: "500" },
});
