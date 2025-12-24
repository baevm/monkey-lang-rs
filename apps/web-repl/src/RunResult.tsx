import { useAtom } from '@reatom/npm-react'
import { isLoadingAtom, resultAtom } from './atoms'

import styles from './App.module.css'
import { Center, Loader, Text } from '@mantine/core'

export const RunResult = () => {
  const [result] = useAtom(resultAtom)
  const [isLoading] = useAtom(isLoadingAtom)

  return (
    <section className={styles.result}>
      {isLoading ? (
        <Center h="100%">
          <Loader />
        </Center>
      ) : result ? (
        <div className={styles.resultContainer}>
          <div className={styles.resultLabel}>
            <Text c="dimmed">Standard output</Text>
          </div>
          <div>{result.output}</div>
          <Text c="gray">Compiled in: {result.time_ms.toFixed(2)} ms</Text>
        </div>
      ) : null}
    </section>
  )
}
