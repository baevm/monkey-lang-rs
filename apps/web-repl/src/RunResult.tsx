import { useAtom } from '@reatom/npm-react'
import { resultAtom } from './atoms'

import styles from './App.module.css'

export const RunResult = () => {
  const [result] = useAtom(resultAtom)

  return (
    <section className={styles.result}>
      {result ? (
        <div>
          <div>------ Standard output ------</div>
          <div>{result}</div>
        </div>
      ) : null}
    </section>
  )
}
