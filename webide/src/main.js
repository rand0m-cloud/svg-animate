import { init } from '@wasmer/wasi'
import { createApp } from 'vue'
import App from './App.vue'

;(async () => {
  await init()
  createApp(App).mount('#app')
})()
