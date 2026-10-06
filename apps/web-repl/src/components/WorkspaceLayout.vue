<script setup lang="ts">
import { computed, ref } from 'vue'
import ResizeHandle from './ResizeHandle.vue'

const DEFAULT_EDITOR_SIZE = 65
const MIN_EDITOR_SIZE = 30
const MAX_EDITOR_SIZE = 80

const container = ref<HTMLElement | null>(null)
const editorSize = ref(DEFAULT_EDITOR_SIZE)

const gridStyle = computed(() => ({
  gridTemplateColumns: `${editorSize.value}% 8px minmax(0, 1fr)`,
}))

function clampSize(value: number) {
  return Math.min(MAX_EDITOR_SIZE, Math.max(MIN_EDITOR_SIZE, value))
}

function resizeFromPointer(clientX: number) {
  const bounds = container.value?.getBoundingClientRect()

  if (!bounds || bounds.width === 0) return

  const percentage = ((clientX - bounds.left) / bounds.width) * 100
  editorSize.value = clampSize(percentage)
}

function nudgeSize(amount: number) {
  if (amount === Number.NEGATIVE_INFINITY) {
    editorSize.value = MIN_EDITOR_SIZE
  } else if (amount === Number.POSITIVE_INFINITY) {
    editorSize.value = MAX_EDITOR_SIZE
  } else {
    editorSize.value = clampSize(editorSize.value + amount)
  }
}

function resetSize() {
  editorSize.value = DEFAULT_EDITOR_SIZE
}
</script>

<template>
  <main ref="container" class="workspace" :style="gridStyle">
    <section class="pane editorPane">
      <slot name="editor" />
    </section>

    <ResizeHandle
      :value="editorSize"
      :min="MIN_EDITOR_SIZE"
      :max="MAX_EDITOR_SIZE"
      @drag="resizeFromPointer"
      @nudge="nudgeSize"
      @reset="resetSize" />

    <section class="pane outputPane">
      <slot name="output" />
    </section>
  </main>
</template>

<style scoped>
.workspace {
  display: grid;
  flex: 1;
  width: 100%;
  min-width: 0;
  min-height: 0;
}

.pane {
  min-width: 0;
  min-height: 0;
  overflow: hidden;
}

@media (max-width: 700px) {
  .workspace {
    display: grid;
    grid-template-rows: minmax(300px, 3fr) minmax(180px, 2fr);
    grid-template-columns: minmax(0, 1fr) !important;
  }

  .outputPane {
    border-top: 1px solid var(--ui-border);
  }
}
</style>
