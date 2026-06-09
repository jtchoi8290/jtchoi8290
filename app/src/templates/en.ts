import { Recipient, Tone } from "../types";

export function greetingEn(recipient: Recipient, recipientName?: string): string {
  const name = recipientName?.trim();
  switch (recipient) {
    case "boss":
      return name ? `Dear ${name},\n\n` : "Hello,\n\n";
    case "client":
      return name ? `Dear ${name},\n\n` : "Dear Sir or Madam,\n\n";
    case "colleague":
      return name ? `Hi ${name},\n\n` : "Hi,\n\n";
    case "external":
      return name ? `Dear ${name},\n\n` : "Hello,\n\n";
  }
}

export function closingEn(tone: Tone, senderName?: string): string {
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

export function toneInstructionEn(tone: Tone, recipient: Recipient): string {
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
