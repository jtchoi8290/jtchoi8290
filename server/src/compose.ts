import Anthropic from "@anthropic-ai/sdk";
import { ComposeRequest, scaffold } from "./templates.js";

const client = new Anthropic();

export interface ComposeResult {
  subject: string;
  body: string;
}

const SYSTEM_PROMPT = `You are an expert business email writer. You write polished, professional emails in either English or Korean.

You will receive:
- The user's intent (what they want to communicate, often casual notes)
- A pre-built greeting and closing
- A tone instruction

Your job: write a business-style email BODY that fits between the greeting and closing, plus a concise subject line.

Critical rules:
- Output ONLY a JSON object with keys "subject" and "body". No prose before or after.
- The body should NOT repeat the greeting or closing — they will be prepended/appended automatically.
- Match the language exactly (Korean → 한국어, English → English). Never mix languages.
- For Korean: use proper honorifics (존댓말) and business register.
- For English: clear, direct, professional. No filler.
- Keep it concise — say what needs saying, nothing more.
- The subject should be specific and scannable (5-10 words).`;

export async function compose(req: ComposeRequest): Promise<ComposeResult> {
  const s = scaffold(req);

  const userMessage = `Language: ${req.language === "ko" ? "Korean (한국어)" : "English"}
Tone instruction: ${s.toneInstruction}
Recipient type: ${req.recipient}
${req.recipientName ? `Recipient name: ${req.recipientName}` : ""}
${req.senderName ? `Sender name: ${req.senderName}` : ""}

User's intent:
${req.intent}

Pre-built greeting (will be prepended — do NOT include in body):
${s.greeting}

Pre-built closing (will be appended — do NOT include in body):
${s.closing}

Write the email subject and body. Respond with JSON only:
{"subject": "...", "body": "..."}`;

  const response = await client.messages.create({
    model: "claude-opus-4-8",
    max_tokens: 1500,
    system: SYSTEM_PROMPT,
    messages: [{ role: "user", content: userMessage }],
    output_config: {
      format: {
        type: "json_schema",
        schema: {
          type: "object",
          properties: {
            subject: { type: "string" },
            body: { type: "string" },
          },
          required: ["subject", "body"],
          additionalProperties: false,
        },
      },
    },
  });

  const textBlock = response.content.find((b) => b.type === "text");
  if (!textBlock || textBlock.type !== "text") {
    throw new Error("Model returned no text");
  }
  const parsed = JSON.parse(textBlock.text) as ComposeResult;

  return {
    subject: parsed.subject.trim(),
    body: `${s.greeting}${parsed.body.trim()}${s.closing}`,
  };
}
