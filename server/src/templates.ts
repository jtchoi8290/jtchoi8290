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

function greetingKo(recipient: Recipient, recipientName?: string): string {
  const name = recipientName?.trim();
  if (name) return `${name}님께,\n\n안녕하세요. `;
  return "안녕하세요. ";
}

function closingKo(tone: Tone, senderName?: string): string {
  const sign = senderName?.trim() ? `\n\n${senderName} 드림` : "\n\n감사합니다.";
  switch (tone) {
    case "formal":
      return `\n\n바쁘신 와중에 검토해 주셔서 감사합니다.${sign}`;
    case "polite":
      return `\n\n감사합니다.${sign}`;
    case "friendly":
      return `\n\n확인 부탁드립니다. 감사합니다!${sign}`;
  }
}

function greetingEn(recipient: Recipient, recipientName?: string): string {
  const name = recipientName?.trim();
  switch (recipient) {
    case "boss":
    case "client":
    case "external":
      return name ? `Dear ${name},\n\n` : "Hello,\n\n";
    case "colleague":
      return name ? `Hi ${name},\n\n` : "Hi,\n\n";
  }
}

function closingEn(tone: Tone, senderName?: string): string {
  const sign = senderName?.trim() ? `\n\nBest regards,\n${senderName}` : "\n\nBest regards,";
  switch (tone) {
    case "formal":
      return `\n\nThank you for your time and consideration.${sign}`;
    case "polite":
      return `\n\nThank you.${sign}`;
    case "friendly":
      return `\n\nThanks so much!${sign}`;
  }
}

function toneInstructionKo(tone: Tone, recipient: Recipient): string {
  const honorific =
    recipient === "boss" || recipient === "client" || recipient === "external"
      ? "최고 수준의 존댓말과 격식체(-습니다/-입니다)를 사용하세요."
      : "정중한 존댓말(-요/-입니다)을 사용하세요.";
  switch (tone) {
    case "formal":
      return `${honorific} 매우 격식 있고 간결한 비즈니스 한국어로 작성하세요.`;
    case "polite":
      return `${honorific} 부드럽고 정중한 어조로 작성하세요.`;
    case "friendly":
      return `${honorific} 따뜻하지만 전문적인 어조로 작성하세요.`;
  }
}

function toneInstructionEn(tone: Tone, recipient: Recipient): string {
  const formality =
    recipient === "boss" || recipient === "client" || recipient === "external"
      ? "Use formal business English."
      : "Use professional but approachable English.";
  switch (tone) {
    case "formal":
      return `${formality} Be concise, precise, and respectful. Avoid contractions.`;
    case "polite":
      return `${formality} Be courteous and clear. Light contractions are fine.`;
    case "friendly":
      return `${formality} Warm and conversational while staying professional.`;
  }
}

export interface Scaffold {
  greeting: string;
  closing: string;
  toneInstruction: string;
}

export function scaffold(req: ComposeRequest): Scaffold {
  if (req.language === "ko") {
    return {
      greeting: greetingKo(req.recipient, req.recipientName),
      closing: closingKo(req.tone, req.senderName),
      toneInstruction: toneInstructionKo(req.tone, req.recipient),
    };
  }
  return {
    greeting: greetingEn(req.recipient, req.recipientName),
    closing: closingEn(req.tone, req.senderName),
    toneInstruction: toneInstructionEn(req.tone, req.recipient),
  };
}
