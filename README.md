# BizEmail

Mobile app that rewrites casual text into business-style emails in Korean or English.

## Stack

- **Frontend**: React Native + Expo (TypeScript)
- **Backend**: Node.js + Express that proxies the Claude API
- **Storage**: On-device history via AsyncStorage
- **Architecture**: Hybrid — fixed greeting/closing templates wrap a Claude-generated subject + body, so honorifics and structure stay consistent while the body is high-quality

## Features

- Korean (한국어) and English
- Tone: formal / polite / friendly
- Recipient context: boss / client / colleague / external
- Auto subject line generation
- Copy to clipboard (subject, body, or full email)
- Local history of past drafts

## Structure

```
app/      Expo React Native app
server/   Express backend that proxies Claude
```

## Run

### Server

```sh
cd server
cp .env.example .env
# edit .env, set ANTHROPIC_API_KEY=sk-ant-...
npm install
npm run dev          # listens on :8787
```

### App

```sh
cd app
npm install
npm start            # opens Expo dev server; scan QR with Expo Go
```

For a physical device, point the app at your machine's LAN IP — edit `app/app.json` → `extra.apiBaseUrl` to e.g. `http://192.168.1.20:8787` (localhost won't reach the laptop from the phone).

## Notes

- The model is `claude-opus-4-8`. To swap (e.g. for cost), edit `server/src/compose.ts`.
- History is capped at 50 entries.
- The backend validates inputs and rejects intents over 4000 chars.
