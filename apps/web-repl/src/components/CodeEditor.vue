<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useColorMode } from '@vueuse/core'
import * as monaco from 'monaco-editor'
import EditorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker'
import { useCodeStore } from '@/stores/useCodeStore'
import { MONKE_LANGUAGE_ID, registerMonkeLanguage } from '@/editor/monkeLanguage'

globalThis.MonacoEnvironment = {
  getWorker() {
    return new EditorWorker()
  },
}

const codeStore = useCodeStore()

const container = ref<HTMLDivElement | null>(null)
const colorMode = useColorMode()

const theme = computed(() => (colorMode.value === 'dark' ? 'vs-dark' : 'vs'))
const MARKER_OWNER = 'monke-diagnostics'

let editor: monaco.editor.IStandaloneCodeEditor | undefined
let changeListener: { dispose(): void } | undefined

function clearMarkers() {
  const model = editor?.getModel()
  if (!model) return

  monaco.editor.setModelMarkers(model, MARKER_OWNER, [])
}

onMounted(() => {
  registerMonkeLanguage()

  editor = monaco.editor.create(container.value!, {
    value: codeStore.code,
    language: MONKE_LANGUAGE_ID,
    theme: theme.value,
    minimap: {
      enabled: false,
    },
    fontSize: 16,
    automaticLayout: true,
  })

  changeListener = editor.onDidChangeModelContent(() => {
    clearMarkers()

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

watch(
  () => codeStore.result?.diagnostics,
  diagnostics => {
    const model = editor?.getModel()
    if (!model) return

    const markers: monaco.editor.IMarkerData[] = (diagnostics ?? []).flatMap(diagnostic => {
      if (diagnostic.line === undefined || diagnostic.column === undefined) {
        return []
      }

      const start = model.validatePosition({
        lineNumber: diagnostic.line,
        column: diagnostic.column,
      })

      return [
        {
          severity:
            diagnostic.severity === 'warning'
              ? monaco.MarkerSeverity.Warning
              : monaco.MarkerSeverity.Error,
          message: `[${diagnostic.phase}] ${diagnostic.message}`,
          startLineNumber: start.lineNumber,
          startColumn: start.column,
          endLineNumber: start.lineNumber,
          endColumn: Math.min(start.column + 1, model.getLineMaxColumn(start.lineNumber)),
        },
      ]
    })

    monaco.editor.setModelMarkers(model, MARKER_OWNER, markers)
  },
)
watch(theme, value => {
  monaco.editor.setTheme(value)
})

onBeforeUnmount(() => {
  clearMarkers()
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
