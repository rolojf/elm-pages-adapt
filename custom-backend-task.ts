import { createClient } from "@libsql/client";
import { randomUUID } from "node:crypto";

export async function hello(name) {
  return `Hello ${name}!`;
}

let _db;
const db = () =>
  (_db ??= createClient({
    url: process.env.TURSO_DATABASE_URL ?? "file:./local-visits.db",
    authToken: process.env.TURSO_AUTH_TOKEN, // undefined is fine for a local file
  }));

async function ensureSchema() {
  await db().execute(`CREATE TABLE IF NOT EXISTS visits (
    id INTEGER PRIMARY KEY AUTOINCREMENT, site TEXT NOT NULL, token TEXT,
    visitor_id TEXT NOT NULL, event TEXT NOT NULL, ts INTEGER NOT NULL,
    ua TEXT, referrer TEXT, country TEXT, region TEXT, city TEXT)`);
}

// logVisit({ site, token, visitorId, event, ts, ua, referrer, country, region, city }) -> { visitorId, count }
export async function logVisit(input) {
  const {
    site,
    token = null,
    visitorId,
    event,
    ts,
    ua = null,
    referrer = null,
    country = null,
    region = null,
    city = null,
  } = input;
  const id = visitorId && visitorId.length ? visitorId : randomUUID(); // mint if absent
  await ensureSchema();
  await db().execute({
    sql: `INSERT INTO visits (site, token, visitor_id, event, ts, ua, referrer, country, region, city)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
    args: [site, token, id, event, ts, ua, referrer, country, region, city],
  });
  const r = await db().execute({
    sql: `SELECT COUNT(*) AS c FROM visits WHERE visitor_id = ?
          AND ((token IS NULL AND ? IS NULL) OR token = ?)`,
    args: [id, token, token],
  });
  return { visitorId: id, count: Number(r.rows[0].c) };
}
