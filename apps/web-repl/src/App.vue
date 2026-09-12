<script setup lang="ts">
import { onMounted } from 'vue'
import CodeEditor from './components/CodeEditor.vue'
import RunResult from './components/RunResult.vue'
import Toolbar from './components/Toolbar.vue'
import { useCodeStore } from './stores/useCodeStore.ts'
import { compilerWorker } from './workerInstance.ts'

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
</script>

<template>
  <UApp>
    <div class="app">
      <Toolbar />

      <main class="main">
        <CodeEditor />
        <RunResult />
      </main>
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

.main {
  display: flex;
  width: 100%;
  flex: 1;
  min-height: 0;
}
</style>
