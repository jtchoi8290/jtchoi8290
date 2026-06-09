import React from "react";
import { Pressable, ScrollView, StyleSheet, Text, View, Alert } from "react-native";
import * as Clipboard from "expo-clipboard";
import { NativeStackScreenProps } from "@react-navigation/native-stack";
import { RootStackParamList } from "../../App";

type Props = NativeStackScreenProps<RootStackParamList, "Result">;

export function ResultScreen({ route }: Props) {
  const { item } = route.params;
  const isKo = item.request.language === "ko";

  const copy = async (text: string, label: string) => {
    await Clipboard.setStringAsync(text);
    Alert.alert(isKo ? "복사됨" : "Copied", label);
  };

  const fullEmail = `${isKo ? "제목" : "Subject"}: ${item.subject}\n\n${item.body}`;

  return (
    <ScrollView style={styles.container} contentContainerStyle={styles.content}>
      <View style={styles.card}>
        <Text style={styles.cardLabel}>{isKo ? "제목" : "Subject"}</Text>
        <Text style={styles.subject}>{item.subject}</Text>
        <Pressable style={styles.copyBtn} onPress={() => copy(item.subject, isKo ? "제목" : "Subject")}>
          <Text style={styles.copyText}>{isKo ? "제목 복사" : "Copy Subject"}</Text>
        </Pressable>
      </View>

      <View style={styles.card}>
        <Text style={styles.cardLabel}>{isKo ? "본문" : "Body"}</Text>
        <Text style={styles.body}>{item.body}</Text>
        <Pressable style={styles.copyBtn} onPress={() => copy(item.body, isKo ? "본문" : "Body")}>
          <Text style={styles.copyText}>{isKo ? "본문 복사" : "Copy Body"}</Text>
        </Pressable>
      </View>

      <Pressable
        style={[styles.copyBtn, styles.copyAll]}
        onPress={() => copy(fullEmail, isKo ? "전체 이메일" : "Full email")}
      >
        <Text style={[styles.copyText, { color: "#fff" }]}>
          {isKo ? "전체 복사" : "Copy All"}
        </Text>
      </Pressable>
    </ScrollView>
  );
}

const styles = StyleSheet.create({
  container: { flex: 1, backgroundColor: "#f9fafb" },
  content: { padding: 16 },
  card: {
    backgroundColor: "#fff",
    borderRadius: 12,
    padding: 16,
    marginBottom: 12,
    borderWidth: 1,
    borderColor: "#e5e7eb",
  },
  cardLabel: { fontSize: 12, fontWeight: "600", color: "#6b7280", marginBottom: 6, textTransform: "uppercase" },
  subject: { fontSize: 17, fontWeight: "600", color: "#111827", marginBottom: 12 },
  body: { fontSize: 15, color: "#1f2937", lineHeight: 22, marginBottom: 12 },
  copyBtn: {
    borderWidth: 1,
    borderColor: "#d1d5db",
    paddingVertical: 10,
    borderRadius: 8,
    alignItems: "center",
  },
  copyText: { color: "#111827", fontWeight: "500", fontSize: 14 },
  copyAll: { backgroundColor: "#111827", borderColor: "#111827", marginTop: 4 },
});
