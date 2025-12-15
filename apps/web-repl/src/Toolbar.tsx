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

const CODE_EXAMPLES = [
  {
    label: 'If expression',
    value: `let a = 10;
let b = 100;

if(a > b) {
    print("a is bigger");
} else {
    print("b is bigger");
}`,
  },
  {
    label: 'Fibonacci function',
    value: `let fibonacci = function(x) {
    if (x == 0) {
        return 0;
    } else {
    if (x == 1) {
            return 1;
        } else {
            return fibonacci(x - 1) + fibonacci(x - 2);
        }
    }
};
let result = fibonacci(25);

print(result);`,
  },
  {
    label: 'Calculation function',
    value: `let calculate = function (x, y) {
  return x + y * 100
}

let result = calculate(5, 4)

print(result)`,
  },
]
