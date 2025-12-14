import { Button, Flex, Group, Radio } from '@mantine/core'
import { useAction, useAtom } from '@reatom/npm-react'

import styles from './App.module.css'
import { runnerModeAtom, workerReadyAtom, runCode, onRunnerModeChange, type Mode } from './atoms'

export const Toolbar = () => {
  const [runnerMode] = useAtom(runnerModeAtom)
  const [isWorkerReady] = useAtom(workerReadyAtom)

  const onRun = useAction(runCode)
  const changeRunnerMode = useAction(onRunnerModeChange)

  return (
    <header className={styles.header}>
      <Flex h="100%" align="center" gap="lg" justify="center">
        <Button onClick={onRun} loading={!isWorkerReady}>
          Run
        </Button>
        <Radio.Group value={runnerMode} onChange={value => changeRunnerMode(value as Mode)}>
          <Group>
            <Radio value="compiler" label="Compiler" />
            <Radio value="interpreter" label="Interpreter" />
          </Group>
        </Radio.Group>
      </Flex>
    </header>
  )
}
