import Constants from "expo-constants";
import { ComposeRequest, ComposeResult } from "../types";

const baseUrl =
  (Constants.expoConfig?.extra as { apiBaseUrl?: string } | undefined)?.apiBaseUrl ??
  "http://localhost:8787";

export async function compose(req: ComposeRequest): Promise<ComposeResult> {
  const res = await fetch(`${baseUrl}/compose`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(req),
  });
  if (!res.ok) {
    const text = await res.text();
    throw new Error(`compose failed (${res.status}): ${text}`);
  }
  return (await res.json()) as ComposeResult;
}
