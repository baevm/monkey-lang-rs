import styles from './App.module.css'
import { useAction } from '@reatom/npm-react'
import { useEffect } from 'react'
import { onError, onResult, onWorkerReady } from './atoms'
import { Toolbar } from './Toolbar'
import { compilerWorker } from './workerInstance'
import { CodeEditor } from './CodeEditor'
import { RunResult } from './RunResult'

export function App() {
  const handleWorkerReady = useAction(onWorkerReady)
  const handleResult = useAction(onResult)
  const handleError = useAction(onError)

  useEffect(() => {
    const handleMessage = (event: MessageEvent) => {
      const { type, result, error } = event.data

      if (type === 'ready') {
        handleWorkerReady()
      } else if (type === 'result') {
        handleResult(result)
      } else if (type === 'error') {
        handleError(error)
      }
    }

    compilerWorker.addEventListener('message', handleMessage)
    return () => compilerWorker.removeEventListener('message', handleMessage)
  }, [])

  return (
    <div className={styles.app}>
      <Toolbar />
      <main className={styles.main}>
        <CodeEditor />
        <RunResult />
      </main>
    </div>
  )
}
