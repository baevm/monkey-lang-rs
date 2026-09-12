import { compilerWorker } from '@/workerInstance'
import { defineStore } from 'pinia'

export type Mode = 'compiler' | 'interpreter'

interface Diagnostic {
  severity: 'error' | 'warning'
  phase: 'parser' | 'compiler' | 'runtime'
  message: string
  line: number | undefined
  column: number | undefined
}

interface RunResult {
  stdout: string
  value: string | undefined
  diagnostics: Diagnostic[]
  durationMs: number
}

interface State {
  code: string | undefined
  runnerMode: Mode
  result: RunResult | null
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

    setResult(result: RunResult) {
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
      this.result = { stdout: error, durationMs: 0, value: undefined, diagnostics: [] }
      this.isRunning = false
    },

    runCode() {
      const code = this.code
      const mode = this.runnerMode

      if (code) {
        this.isRunning = true
        this.result = null
        compilerWorker.postMessage({ code, mode })
      }
    },
  },
})
