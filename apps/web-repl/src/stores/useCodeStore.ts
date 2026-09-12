import { compilerWorker } from '@/workerInstance'
import { defineStore } from 'pinia'

export type Mode = 'compiler' | 'interpreter'

interface CompileResult {
  output: string
  time_ms: number
}

interface State {
  code: string | undefined
  runnerMode: Mode
  result: CompileResult | null
  workerReady: boolean
  isRunning: boolean
}

export const useCodeStore = defineStore('code', {
  state: (): State => ({
    code: '',
    runnerMode: 'compiler',
    result: null,
    workerReady: false,
    isRunning: false,
  }),

  getters: {
    isLoading(state) {
      return state.isRunning || !state.workerReady
    },
  },

  actions: {
    setWorkerReady() {
      this.workerReady = true
    },

    setResult(result: CompileResult) {
      this.result = result
      this.isRunning = false
    },

    changeCode(code: string | undefined) {
      this.code = code
    },

    changeCodeExample(code: string) {
      this.code = code
    },

    setError(error: string) {
      this.result = { output: error, time_ms: 0 }
      this.isRunning = false
    },

    runCode() {
      const code = this.code
      const mode = this.runnerMode
      const workerReady = this.workerReady

      if (!workerReady) {
        this.result = { output: 'WASM is still loading...', time_ms: 0 }
        return
      }

      if (code) {
        this.isRunning = true
        this.result = null
        compilerWorker.postMessage({ code, mode })
      }
    },
  },
})
