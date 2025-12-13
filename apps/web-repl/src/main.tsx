import { createRoot } from 'react-dom/client'
import { App } from './App.tsx'
import { MantineProvider } from '@mantine/core'
import { reatomContext } from '@reatom/npm-react'
import '@mantine/core/styles.css'
import { createCtx } from '@reatom/framework'

const ctx = createCtx()

createRoot(document.getElementById('root')!).render(
  <reatomContext.Provider value={ctx}>
    <MantineProvider>
      <App />
    </MantineProvider>
  </reatomContext.Provider>,
)
