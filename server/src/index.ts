import express, { Request, Response } from "express";
import cors from "cors";
import { compose } from "./compose.js";
import { ComposeRequest } from "./templates.js";

const app = express();
app.use(cors());
app.use(express.json({ limit: "100kb" }));

app.get("/health", (_req, res) => {
  res.json({ ok: true });
});

app.post("/compose", async (req: Request, res: Response) => {
  try {
    const body = req.body as Partial<ComposeRequest>;
    if (!body.language || (body.language !== "ko" && body.language !== "en")) {
      return res.status(400).json({ error: "invalid language" });
    }
    if (!body.tone || !["formal", "polite", "friendly"].includes(body.tone)) {
      return res.status(400).json({ error: "invalid tone" });
    }
    if (!body.recipient || !["boss", "client", "colleague", "external"].includes(body.recipient)) {
      return res.status(400).json({ error: "invalid recipient" });
    }
    if (!body.intent || typeof body.intent !== "string" || !body.intent.trim()) {
      return res.status(400).json({ error: "intent is required" });
    }
    if (body.intent.length > 4000) {
      return res.status(400).json({ error: "intent too long (max 4000 chars)" });
    }

    const result = await compose(body as ComposeRequest);
    res.json(result);
  } catch (e) {
    console.error("compose error:", e);
    const msg = e instanceof Error ? e.message : "unknown error";
    res.status(500).json({ error: msg });
  }
});

const port = Number(process.env.PORT ?? 8787);
app.listen(port, () => {
  console.log(`biz-email-server listening on :${port}`);
});
