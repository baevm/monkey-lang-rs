<script setup lang="ts">
import { onMounted } from 'vue'
import CodeEditor from './components/CodeEditor.vue'
import RunResult from './components/RunResult.vue'
import Toolbar from './components/Toolbar.vue'
import { useCodeStore } from './stores/useCodeStore.ts'
import { compilerWorker } from './workerInstance.ts'
import { useHotKeys } from './composables/useHotKeys.tsx'
import WorkspaceLayout from './components/WorkspaceLayout.vue'

const codeStore = useCodeStore()

onMounted(() => {
  const handleMessage = (event: MessageEvent) => {
    const { type, result, error } = event.data

    if (type === 'ready') {
      codeStore.setWorkerReady()
    } else if (type === 'result') {
      codeStore.setResult(result)
    } else if (type === 'error') {
      codeStore.setError(error)
    }
  }

  compilerWorker.addEventListener('message', handleMessage)
})

useHotKeys(['Meta', 'Enter'], codeStore.runCode)
useHotKeys(['Control', 'Enter'], codeStore.runCode)
</script>

<template>
  <UApp>
    <div class="app">
      <Toolbar />

      <WorkspaceLayout>
        <template #editor>
          <CodeEditor />
        </template>
        <template #output>
          <RunResult />
        </template>
      </WorkspaceLayout>
    </div>
  </UApp>
</template>

<style scoped>
.app {
  width: 100%;
  height: 100vh;
  display: flex;
  flex-direction: column;
}
</style>
