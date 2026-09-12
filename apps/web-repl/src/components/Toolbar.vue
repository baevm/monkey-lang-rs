<script setup lang="ts">
import { CODE_EXAMPLES } from './codeExamples'
import ThemeButton from './ThemeButton.vue'
import { useCodeStore, type Mode } from '@/stores/useCodeStore.ts'
import type { DropdownMenuItem } from '@nuxt/ui'

const codeStore = useCodeStore()

const radioItems: Array<{ label: string; value: Mode }> = [
  { label: 'Compiler', value: 'compiler' },
  { label: 'Interpreter', value: 'interpreter' },
]

const codeExampleItems: DropdownMenuItem[] = CODE_EXAMPLES.map(example => ({
  label: example.label,
  onSelect: () => codeStore.changeCodeExample(example.value),
}))
</script>

<template>
  <header class="header">
    <div class="toolbar">
      <div />

      <div class="centerToolbar">
        <UDropdownMenu :items="codeExampleItems">
          <UButton
            label="Code examples"
            trailing-icon="i-lucide-chevron-down"
            color="neutral"
            variant="outline" />
        </UDropdownMenu>

        <UButton
          @click="codeStore.runCode()"
          :loading="codeStore.isLoading"
          :disabled="!codeStore.code">
          Run
        </UButton>

        <URadioGroup v-model="codeStore.runnerMode" :items="radioItems" orientation="horizontal" />
      </div>

      <ThemeButton />
    </div>
  </header>
</template>

<style lang="css" scoped>
.header {
  height: 60px;
  border-bottom: 1px solid var(--ui-border);
}

.toolbar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  flex-wrap: wrap;
  gap: 0.5rem;
  height: 100%;
  padding: 0.5rem;
}

.centerToolbar {
  display: flex;
  height: 100%;
  align-items: center;
  gap: 0.5rem;
}

.codeExamplesSelect {
  width: 160px;
}
</style>
