module.exports = {
  content: ["./src/**/*.{html,js,jsx,ts,tsx,elm}"],
    daisyui: {
      themes: ["light", "dark", "cupcake"],
    },
    theme: {
    extend: {
      keyframes: {
        fadeInUp: {
          '0%': { opacity: '0', transform: 'translateY(20px)' },
          '100%': { opacity: '1', transform: 'translateY(0)' },
        },
        slideRotate: {
          '0%': {
            transform: 'translateX(20px) rotate(5deg)',
            opacity: '0.5',
          },
          '100%': {
            transform: 'translateX(0) rotate(0deg)',
            opacity: '1',
          },
        },
        float: {
          '0%, 100%': { transform: 'translate(0, 0)' },
          '50%': { transform: 'translate(0, 8px)' },
        }
      },
      animation: {
        fadeInUp: 'fadeInUp 500ms ease-out',
        slideRotate: 'slideRotate 250ms ease-out',
        float: 'float 3s ease-in-out 0ms infinite',
        fadeInThenFloat: 'fadeInUp 500ms ease-out, float 2s infinite 500ms'
      },
    },
  },
  plugins: [
    require('@tailwindcss/typography'),
    require("daisyui")
  ],
}