import { Editor } from '@monaco-editor/react'
import { useAtom, useAction } from '@reatom/npm-react'
import { codeAtom, onCodeChange } from './atoms'

import styles from './App.module.css'

export const CodeEditor = () => {
  const [code] = useAtom(codeAtom)
  const handleChange = useAction(onCodeChange)

  return (
    <section className={styles.code}>
      <Editor
        height="100%"
        options={{ minimap: { enabled: false }, fontSize: 16 }}
        value={code}
        onChange={handleChange}
      />
    </section>
  )
}
