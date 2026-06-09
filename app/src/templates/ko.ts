import { Recipient, Tone } from "../types";

export function greetingKo(recipient: Recipient, recipientName?: string): string {
  const name = recipientName?.trim();
  switch (recipient) {
    case "boss":
      return name ? `${name}님께,\n\n안녕하세요. ` : "안녕하세요. ";
    case "client":
      return name ? `${name}님께,\n\n안녕하세요. ` : "안녕하세요. ";
    case "colleague":
      return name ? `${name}님,\n\n안녕하세요. ` : "안녕하세요. ";
    case "external":
      return name ? `${name}님께,\n\n안녕하세요. ` : "안녕하세요. ";
  }
}

export function closingKo(tone: Tone, senderName?: string): string {
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

export function toneInstructionKo(tone: Tone, recipient: Recipient): string {
  const honorific =
    recipient === "boss" || recipient === "client" || recipient === "external"
      ? "최고 수준의 존댓말과 격식체(-습니다/-입니다)를 사용하세요."
      : "정중한 존댓말(-요/-입니다)을 사용하세요.";
  switch (tone) {
    case "formal":
      return `${honorific} 매우 격식 있고 간결한 비즈니스 한국어로 작성하세요. 군더더기 없이 명확하게.`;
    case "polite":
      return `${honorific} 부드럽고 정중한 어조로 작성하세요.`;
    case "friendly":
      return `${honorific} 따뜻하지만 전문적인 어조로 작성하세요.`;
  }
}
