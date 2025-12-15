import { useAtom } from '@reatom/npm-react'
import { resultAtom } from './atoms'

import styles from './App.module.css'
import { Text } from '@mantine/core'

export const RunResult = () => {
  const [result] = useAtom(resultAtom)

  return (
    <section className={styles.result}>
      {result ? (
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
