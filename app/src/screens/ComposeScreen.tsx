import React, { useState } from "react";
import {
  ActivityIndicator,
  Alert,
  KeyboardAvoidingView,
  Platform,
  Pressable,
  ScrollView,
  StyleSheet,
  Text,
  TextInput,
  View,
} from "react-native";
import { NativeStackScreenProps } from "@react-navigation/native-stack";
import { Segmented } from "../components/Segmented";
import { compose } from "../api/client";
import { addHistory } from "../storage/history";
import { ComposeRequest, Language, Recipient, Tone, HistoryItem } from "../types";
import { RootStackParamList } from "../../App";

type Props = NativeStackScreenProps<RootStackParamList, "Compose">;

const LABELS = {
  ko: {
    language: "언어",
    tone: "톤",
    toneOpts: { formal: "격식", polite: "정중", friendly: "친근" },
    recipient: "수신인",
    recipientOpts: { boss: "상사", client: "고객", colleague: "동료", external: "외부" },
    recipientName: "수신인 이름 (선택)",
    senderName: "발신인 이름 (선택)",
    intent: "전달하고 싶은 내용",
    intentPlaceholder: "예: 내일 회의 30분 늦을 것 같다고 알림",
    submit: "이메일 작성",
    history: "기록",
  },
  en: {
    language: "Language",
    tone: "Tone",
    toneOpts: { formal: "Formal", polite: "Polite", friendly: "Friendly" },
    recipient: "Recipient",
    recipientOpts: { boss: "Boss", client: "Client", colleague: "Colleague", external: "External" },
    recipientName: "Recipient name (optional)",
    senderName: "Your name (optional)",
    intent: "What do you want to say?",
    intentPlaceholder: "e.g., let them know I'll be 30 min late to tomorrow's meeting",
    submit: "Compose Email",
    history: "History",
  },
} as const;

export function ComposeScreen({ navigation }: Props) {
  const [language, setLanguage] = useState<Language>("en");
  const [tone, setTone] = useState<Tone>("polite");
  const [recipient, setRecipient] = useState<Recipient>("client");
  const [recipientName, setRecipientName] = useState("");
  const [senderName, setSenderName] = useState("");
  const [intent, setIntent] = useState("");
  const [loading, setLoading] = useState(false);

  const t = LABELS[language];

  const onSubmit = async () => {
    if (!intent.trim()) {
      Alert.alert(language === "ko" ? "내용을 입력하세요" : "Please enter your message");
      return;
    }
    const req: ComposeRequest = {
      language,
      tone,
      recipient,
      recipientName: recipientName.trim() || undefined,
      senderName: senderName.trim() || undefined,
      intent: intent.trim(),
    };
    setLoading(true);
    try {
      const result = await compose(req);
      const item: HistoryItem = {
        id: String(Date.now()),
        createdAt: Date.now(),
        request: req,
        ...result,
      };
      await addHistory(item);
      navigation.navigate("Result", { item });
    } catch (e) {
      Alert.alert("Error", e instanceof Error ? e.message : String(e));
    } finally {
      setLoading(false);
    }
  };

  return (
    <KeyboardAvoidingView
      style={{ flex: 1 }}
      behavior={Platform.OS === "ios" ? "padding" : undefined}
    >
      <ScrollView style={styles.container} contentContainerStyle={styles.content}>
        <View style={styles.section}>
          <Text style={styles.label}>{t.language}</Text>
          <Segmented
            value={language}
            onChange={setLanguage}
            options={[
              { value: "en", label: "English" },
              { value: "ko", label: "한국어" },
            ]}
          />
        </View>

        <View style={styles.section}>
          <Text style={styles.label}>{t.tone}</Text>
          <Segmented
            value={tone}
            onChange={setTone}
            options={[
              { value: "formal", label: t.toneOpts.formal },
              { value: "polite", label: t.toneOpts.polite },
              { value: "friendly", label: t.toneOpts.friendly },
            ]}
          />
        </View>

        <View style={styles.section}>
          <Text style={styles.label}>{t.recipient}</Text>
          <Segmented
            value={recipient}
            onChange={setRecipient}
            options={[
              { value: "boss", label: t.recipientOpts.boss },
              { value: "client", label: t.recipientOpts.client },
              { value: "colleague", label: t.recipientOpts.colleague },
              { value: "external", label: t.recipientOpts.external },
            ]}
          />
        </View>

        <View style={styles.section}>
          <Text style={styles.label}>{t.recipientName}</Text>
          <TextInput
            style={styles.input}
            value={recipientName}
            onChangeText={setRecipientName}
            placeholder={language === "ko" ? "예: 김민수" : "e.g., Alex Kim"}
            placeholderTextColor="#9ca3af"
          />
        </View>

        <View style={styles.section}>
          <Text style={styles.label}>{t.senderName}</Text>
          <TextInput
            style={styles.input}
            value={senderName}
            onChangeText={setSenderName}
            placeholder={language === "ko" ? "예: 홍길동" : "e.g., Sam Park"}
            placeholderTextColor="#9ca3af"
          />
        </View>

        <View style={styles.section}>
          <Text style={styles.label}>{t.intent}</Text>
          <TextInput
            style={[styles.input, styles.textarea]}
            value={intent}
            onChangeText={setIntent}
            placeholder={t.intentPlaceholder}
            placeholderTextColor="#9ca3af"
            multiline
            numberOfLines={5}
            textAlignVertical="top"
          />
        </View>

        <Pressable
          onPress={onSubmit}
          disabled={loading}
          style={[styles.submit, loading && { opacity: 0.6 }]}
        >
          {loading ? (
            <ActivityIndicator color="#fff" />
          ) : (
            <Text style={styles.submitText}>{t.submit}</Text>
          )}
        </Pressable>

        <Pressable style={styles.secondary} onPress={() => navigation.navigate("History")}>
          <Text style={styles.secondaryText}>{t.history}</Text>
        </Pressable>
      </ScrollView>
    </KeyboardAvoidingView>
  );
}

const styles = StyleSheet.create({
  container: { flex: 1, backgroundColor: "#fff" },
  content: { padding: 16, paddingBottom: 32 },
  section: { marginBottom: 18 },
  label: { fontSize: 13, fontWeight: "600", color: "#374151", marginBottom: 8 },
  input: {
    borderWidth: 1,
    borderColor: "#e5e7eb",
    borderRadius: 8,
    paddingHorizontal: 12,
    paddingVertical: 10,
    fontSize: 15,
    color: "#111827",
    backgroundColor: "#fff",
  },
  textarea: { minHeight: 110 },
  submit: {
    backgroundColor: "#111827",
    paddingVertical: 14,
    borderRadius: 10,
    alignItems: "center",
    marginTop: 8,
  },
  submitText: { color: "#fff", fontWeight: "600", fontSize: 16 },
  secondary: { paddingVertical: 12, alignItems: "center", marginTop: 8 },
  secondaryText: { color: "#4b5563", fontSize: 14 },
});
