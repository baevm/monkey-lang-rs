import {
  ActionIcon,
  Button,
  Flex,
  Group,
  Radio,
  Select,
  useMantineColorScheme,
} from '@mantine/core'
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
import { IconSun, IconSunOff } from '@tabler/icons-react'

export const Toolbar = () => {
  const [runnerMode] = useAtom(runnerModeAtom)
  const [isLoading] = useAtom(isLoadingAtom)

  const run = useAction(runCode)
  const changeRunnerMode = useAction(onRunnerModeChange)
  const changeExample = useAction(onChangeCodeExample)

  return (
    <header className={styles.header}>
      <Flex px="lg" h="100%" w="100%" align="center" justify="space-between">
        <div />
        <Flex h="100%" align="center" gap="lg">
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

        <ThemeButton />
      </Flex>
    </header>
  )
}

const ThemeButton = () => {
  const { colorScheme, toggleColorScheme } = useMantineColorScheme()

  return (
    <ActionIcon onClick={toggleColorScheme} variant="transparent">
      {colorScheme === 'light' ? <IconSunOff /> : <IconSun />}
    </ActionIcon>
  )
}
