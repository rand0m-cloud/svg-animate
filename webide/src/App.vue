<script setup>
import { onMounted, ref, reactive, watch } from 'vue'
import { WASI } from '@wasmer/wasi'

const wasm = ref(null)
const files = reactive({})
const base_content = ref("");
const frames = ref([]);
const frame_index = ref(0);

const render = async () => {
    let wasi = new WASI({
        args: ['svg-animate', '--height', '320', '--width', '320', "--framerate","30" ,'base.anim', 'out']
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
        let render_frame = wasi.getStdoutString();
        frames.value = render_frame.split("</svg>").map((x)=>x + "</svg>")
        console.warn(wasi.getStderrString())
    }
}

onMounted(async () => {
    const moduleBytes = fetch(`${location.origin}/svg_animate.wasm`)
    wasm.value = await WebAssembly.compileStreaming(moduleBytes)
    const std = await fetch(`${location.origin}/std.anim`).then(r=>r.text());
    const base = await fetch(`${location.origin}/base.anim`).then(r=>r.text());
    files["std.anim"] = std;
    base_content.value = base;

    function rafLoop() {
        requestAnimationFrame(() => {
            if (frames.value.length > 0) {
                frame_index.value += 1;
                
                frame_index.value %= frames.value.length;
            }
                rafLoop();
        })
    }
    rafLoop()
})

watch(base_content, () => {
    files["base.anim"] = base_content.value;
})
</script>

<template>
  <main>
    <button @click="render">Render</button>
    <textarea v-model="base_content" />
    <div v-if="frames[frame_index]" v-html="frames[frame_index]" />
  </main>
</template>
