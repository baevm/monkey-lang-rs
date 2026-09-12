<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useColorMode } from '@vueuse/core'
import * as monaco from 'monaco-editor'
import EditorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker'
import { useCodeStore } from '@/stores/useCodeStore'

globalThis.MonacoEnvironment = {
  getWorker() {
    return new EditorWorker()
  },
}

const codeStore = useCodeStore()

const container = ref<HTMLDivElement | null>(null)
const colorMode = useColorMode()

const theme = computed(() => (colorMode.value === 'dark' ? 'vs-dark' : 'vs'))

let editor: monaco.editor.IStandaloneCodeEditor | undefined
let changeListener: { dispose(): void } | undefined

onMounted(() => {
  editor = monaco.editor.create(container.value!, {
    value: codeStore.code,
    language: 'plaintext',
    theme: theme.value,
    minimap: {
      enabled: false,
    },
    fontSize: 16,
    automaticLayout: true,
  })

  changeListener = editor.onDidChangeModelContent(() => {
    const value = editor?.getValue() ?? ''

    if (value !== codeStore.code) {
      codeStore.changeCode(value)
    }
  })
})

watch(
  () => codeStore.code,
  value => {
    if (editor && value !== editor.getValue()) {
      editor.setValue(value ?? '')
    }
  },
)

watch(theme, value => {
  monaco.editor.setTheme(value)
})

onBeforeUnmount(() => {
  changeListener?.dispose()
  editor?.dispose()
})
</script>

<template>
  <div ref="container" class="codeEditor" />
</template>

<style scoped>
.codeEditor {
  flex: 2;
  height: 100%;
  border-right: 1px solid var(--ui-border);
  overflow: auto;
}
</style>
