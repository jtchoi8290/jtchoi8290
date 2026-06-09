export type Language = "ko" | "en";

export type Tone = "formal" | "polite" | "friendly";

export type Recipient = "boss" | "client" | "colleague" | "external";

export interface ComposeRequest {
  language: Language;
  tone: Tone;
  recipient: Recipient;
  recipientName?: string;
  senderName?: string;
  intent: string;
}

export interface ComposeResult {
  subject: string;
  body: string;
}

export interface HistoryItem extends ComposeResult {
  id: string;
  createdAt: number;
  request: ComposeRequest;
}
