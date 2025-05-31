module.exports = {
  content: ["./src/**/*.{html,js,jsx,ts,tsx,elm}"],
  theme: {
    extend: {},
  },
  plugins: [require("@tailwindcss/typography"), require("daisyui")],
}