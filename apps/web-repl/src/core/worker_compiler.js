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
    const result = mode === 'compiler' ? compile_code(code) : interpret_code(code)
    self.postMessage({ type: 'result', result })
  } catch (error) {
    self.postMessage({ type: 'error', error: error.message })
  }
}
