import { defineConfig } from "vite";
import { ViteWebfontDownload } from "vite-plugin-webfont-dl";
import adapter from "elm-pages/adapter/netlify.js";
import tailwindcss from "@tailwindcss/vite";

export default {
   vite: defineConfig({
      plugins: [
         tailwindcss(),
         ViteWebfontDownload([
            "https://fonts.googleapis.com/css2?family=Fira+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap&subset=latin,latin-ext",
            "https://fonts.googleapis.com/css2?family=Roboto+Slab:wght@100;200;300;400;500;600;700;800;900&display=swap&subset=latin,latin-ext",
         ]),
      ],
   }),
  adapter,
  headTagsTemplate(context) {
    return `
<meta name="generator" content="elm-pages v${context.cliVersion}" />
`;
  },
  preloadTagForFile(file) {
    return !file.endsWith(".css");
  },
};
