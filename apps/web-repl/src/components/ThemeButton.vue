<script setup lang="ts">
import { useColorMode } from '@vueuse/core'

const colorMode = useColorMode()

const changeColorMode = () => {
  if (colorMode.value === 'dark') {
    colorMode.value = 'light'
  } else {
    colorMode.value = 'dark'
  }
}
</script>

<template>
  <UButton
    variant="ghost"
    :icon="colorMode === 'dark' ? 'i-lucide-moon' : 'i-lucide-sun'"
    @click="changeColorMode"></UButton>
</template>
