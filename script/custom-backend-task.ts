import { resolve } from "node:path";
import { __unstable__loadDesignSystem } from "@tailwindcss/node";

export async function hello(name: string) {
  return `Hello ${name}!`;
}

// Returns the subset of `candidates` that generate no CSS (i.e. are not valid Tailwind
// utilities). `base` is resolved against process.cwd(); the lint runs from the project
// root and passes ".", so `@config "./tailwind.config.cjs"` resolves to the real config.
//
// NOTE: __unstable__loadDesignSystem is an unstable, version-pinned API of
// @tailwindcss/node (currently 4.3.1). If Tailwind is upgraded, re-verify the export and
// candidatesToCss against node_modules/@tailwindcss/node/dist/index.d.ts.
export async function validateTwClasses({
  candidates,
  base,
}: {
  candidates: string[];
  base: string;
}) {
  const css = `@import "tailwindcss";\n@config "./tailwind.config.cjs";`;
  const ds = await __unstable__loadDesignSystem(css, {
    base: resolve(base),
  });
  const results = ds.candidatesToCss(candidates);
  return candidates.filter((_, i) => results[i] === null);
}
