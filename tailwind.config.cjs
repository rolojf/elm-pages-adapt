module.exports = {
   theme: {
      extend: {
         fontFamily: {
            sans: [
               '"Fira Sans"',
               "system-ui",
               "-apple-system",
               "BlinkMacSystemFont",
               '"Segoe UI"',
               "Roboto",
               '"Helvetica Neue"',
               "Arial",
               '"Noto Sans"',
               "sans-serif",
               '"Apple Color Emoji"',
               '"Segoe UI Emoji"',
               '"Segoe UI Symbol"',
               '"Noto Color Emoji"',
            ],
            serif: ['"Roboto Slab"', "Georgia", "Cambria", '"Times New Roman"', "Times", "serif"],
         },
      },
   },
   plugins: [
      require("@tailwindcss/typography"),
      require("@tailwindcss/forms"),
   ],
};
