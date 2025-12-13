import { action, atom } from '@reatom/framework'
import { compilerWorker } from './workerInstance'

type Mode = 'compiler' | 'interpreter'

export const codeAtom = atom<string | undefined>('', 'codeAtom')
export const runnerModeAtom = atom<Mode>('compiler', 'runnerModeAtom')
export const resultAtom = atom<string>('', 'resultAtom')
export const workerReadyAtom = atom<boolean>(false, 'workerReadyAtom')

export const onWorkerReady = action(ctx => {
  workerReadyAtom(ctx, true)
}, 'onWorkerReady')

export const onResult = action((ctx, result: string) => {
  resultAtom(ctx, result)
}, 'onResult')

export const onError = action((ctx, error: string) => {
  resultAtom(ctx, `Error: ${error}`)
}, 'onError')

export const onRunnerModeChange = action(
  (ctx, newValue: Mode) => runnerModeAtom(ctx, newValue),
  'onRunnerModeChange',
)

export const onCodeChange = action(
  (ctx, newCode: string | undefined) => codeAtom(ctx, newCode),
  'onCodeChange',
)

export const runCode = action(ctx => {
  const code = ctx.get(codeAtom)
  const mode = ctx.get(runnerModeAtom)
  const workerReady = ctx.get(workerReadyAtom)

  if (!workerReady) {
    resultAtom(ctx, 'WASM is still loading...')
    return
  }

  if (code) {
    compilerWorker.postMessage({ code, mode })
  }
}, 'runCode')
