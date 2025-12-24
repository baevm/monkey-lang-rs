import init, { compile_code, interpret_code } from './wasm_compiler/wasm'

let isInitialized = false

init().then(() => {
  isInitialized = true
  self.postMessage({ type: 'ready' })
})

self.onmessage = event => {
  const { code, mode } = event.data

  if (!isInitialized) {
    self.postMessage({ type: 'error', error: 'WASM not initialized' })
    return
  }

  try {
    const startTime = performance.now()

    const wasmResult = mode === 'compiler' ? compile_code(code) : interpret_code(code)
    const endTime = performance.now()

    const result = {
      output: wasmResult,
      time_ms: endTime - startTime,
    }

    self.postMessage({ type: 'result', result })
  } catch (error) {
    console.error('Worker error:', error)
    self.postMessage({ type: 'error', error: error.message || String(error) })
  }
}
