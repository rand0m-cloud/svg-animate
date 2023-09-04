<script setup>
import { onMounted, ref, reactive } from 'vue'
import { WASI } from '@wasmer/wasi'

const wasm = ref(null)
const files = reactive({
  'std.anim':`square(size) { return <rect :width="size" :height="size" fill="red" /> }`}
)
const base_content = ref("");

const render = async (time) => {
  let wasi = new WASI({
    args: ['svg-animate', '--height', '1080', '--width', '1920', 'base.anim', 'out']
  })

  await wasi.instantiate(wasm.value, {})

  for (var filename in files) {
    let file = wasi.fs.open(filename, { create: true, read: true, write: true })
    file.writeString(files[filename])
  }

  // Run the start function
  try {
    wasi.start()
  } catch (e) {
    console.error('crashed')
  } finally {
    console.log(wasi.getStdoutString())
    console.log(wasi.getStderrString())
  }
}

onMounted(async () => {
  const moduleBytes = fetch(`${location.origin}/svg_animate.wasm`)
  wasm.value = await WebAssembly.compileStreaming(moduleBytes)
  files['base.anim']= 'use std\nsprite=square(50)\nanimate 10s sprite';
  await render(0.0)
})
</script>

<template>
  <main>
    <button @click="render(0, files)">Render</button>
  </main>
</template>
