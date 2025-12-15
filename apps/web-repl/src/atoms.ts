import { action, atom } from '@reatom/framework'
import { compilerWorker } from './workerInstance'

export type Mode = 'compiler' | 'interpreter'

export const codeAtom = atom<string | undefined>('', 'codeAtom')
export const runnerModeAtom = atom<Mode>('compiler', 'runnerModeAtom')
export const resultAtom = atom<string>('', 'resultAtom')
const workerReadyAtom = atom<boolean>(false, 'workerReadyAtom')
const isRunningAtom = atom<boolean>(false, 'isRunning')

/** Toolbar "RUN" button loading state */
export const isLoadingAtom = atom<boolean>(
  ctx => ctx.spy(isRunningAtom) || !ctx.spy(workerReadyAtom),
  'isLoadingAtom',
)

export const onWorkerReady = action(ctx => {
  workerReadyAtom(ctx, true)
}, 'onWorkerReady')

export const onResult = action((ctx, result: string) => {
  resultAtom(ctx, result)
  isRunningAtom(ctx, false)
}, 'onResult')

export const onError = action((ctx, error: string) => {
  resultAtom(ctx, `Error: ${error}`)
  isRunningAtom(ctx, false)
}, 'onError')

export const onRunnerModeChange = action(
  (ctx, newValue: Mode) => runnerModeAtom(ctx, newValue),
  'onRunnerModeChange',
)

export const onCodeChange = action(
  (ctx, newCode: string | undefined) => codeAtom(ctx, newCode),
  'onCodeChange',
)

export const onChangeCodeExample = action(
  (ctx, codeExample: string) => codeAtom(ctx, codeExample),
  'onChangeCodeExample',
)

export const runCode = action(ctx => {
  const code = ctx.get(codeAtom)
  const mode = ctx.get(runnerModeAtom)
  const workerReady = ctx.get(workerReadyAtom)

  isRunningAtom(ctx, true)

  if (!workerReady) {
    resultAtom(ctx, 'WASM is still loading...')
    return
  }

  if (code) {
    compilerWorker.postMessage({ code, mode })
  }
}, 'runCode')
