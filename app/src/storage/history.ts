import AsyncStorage from "@react-native-async-storage/async-storage";
import { HistoryItem } from "../types";

const KEY = "biz-email:history";
const MAX_ITEMS = 50;

export async function loadHistory(): Promise<HistoryItem[]> {
  const raw = await AsyncStorage.getItem(KEY);
  if (!raw) return [];
  try {
    return JSON.parse(raw) as HistoryItem[];
  } catch {
    return [];
  }
}

export async function saveHistory(items: HistoryItem[]): Promise<void> {
  await AsyncStorage.setItem(KEY, JSON.stringify(items.slice(0, MAX_ITEMS)));
}

export async function addHistory(item: HistoryItem): Promise<HistoryItem[]> {
  const items = await loadHistory();
  const next = [item, ...items].slice(0, MAX_ITEMS);
  await saveHistory(next);
  return next;
}

export async function clearHistory(): Promise<void> {
  await AsyncStorage.removeItem(KEY);
}
