import { Button, Flex, Group, Radio, Select } from '@mantine/core'
import { useAction, useAtom } from '@reatom/npm-react'

import styles from './App.module.css'
import {
  runnerModeAtom,
  runCode,
  onRunnerModeChange,
  type Mode,
  isLoadingAtom,
  onChangeCodeExample,
} from './atoms'
import { CODE_EXAMPLES } from './codeExamples'

export const Toolbar = () => {
  const [runnerMode] = useAtom(runnerModeAtom)
  const [isLoading] = useAtom(isLoadingAtom)

  const run = useAction(runCode)
  const changeRunnerMode = useAction(onRunnerModeChange)
  const changeExample = useAction(onChangeCodeExample)

  return (
    <header className={styles.header}>
      <Flex h="100%" align="center" gap="lg" justify="center">
        <Select
          data={CODE_EXAMPLES}
          placeholder="Code examples"
          value={null}
          defaultValue=""
          style={{ width: '160px' }}
          onChange={value => {
            if (value) {
              changeExample(value)
            }
          }}
        />
        <Button onClick={run} loading={isLoading}>
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
