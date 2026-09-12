<script setup lang="ts">
import { useCodeStore } from '@/stores/useCodeStore'

const codeStore = useCodeStore()
</script>

<template>
  <section class="runResult">
    <div v-if="codeStore.isLoading" class="loader">
      <UIcon name="i-lucide-loader-circle" class="size-6 animate-spin" />
    </div>

    <div v-else-if="codeStore.result !== null" class="resultContainer">
      <div class="resultLabel">Standard output</div>
      <div class="resultOutput">{{ codeStore.result.output }}</div>
      <div class="compileTime">Compiled in: {{ codeStore.result.time_ms.toFixed(2) }} ms</div>
    </div>
  </section>
</template>

<style lang="css" scoped>
.runResult {
  flex: 1;
  height: 100%;
  overflow: auto;
  background-color: var(--ui-bg-muted);
}

.loader {
  display: flex;
  align-items: center;
  justify-content: center;
  height: 100%;
}

.resultContainer {
  width: 100%;
}

.resultLabel {
  display: flex;
  color: var(--ui-text-dimmed);
}

.resultLabel::before,
.resultLabel::after {
  border-top: 1px solid var(--ui-border);
  content: '';
  flex: 1 1 auto;
  margin: auto;
}

.resultLabel::before {
  margin-right: 16px;
}

.resultLabel::after {
  margin-left: 16px;
}

.resultOutput {
  white-space: pre-wrap;
  overflow-wrap: anywhere;
}

.compileTime {
  color: var(--ui-text-muted);
}
</style>
