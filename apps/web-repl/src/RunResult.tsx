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
          <div>{result}</div>
        </div>
      ) : null}
    </section>
  )
}
