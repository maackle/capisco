exports.config = {
  title: 'Pux Starter App',
  apiBase: 'http://localhost:3000/',
  publicPath: process.env.NODE_ENV === 'production'
               ? '/dist/'
               : 'http://localhost:8080/dist/'
}
