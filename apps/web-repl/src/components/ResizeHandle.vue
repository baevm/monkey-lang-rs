<script setup lang="ts">
import { ref } from 'vue'

defineProps<{
  value: number
  min: number
  max: number
}>()

const emit = defineEmits<{
  drag: [clientX: number]
  nudge: [amount: number]
  reset: []
}>()

const dragging = ref(false)

function startDrag(event: PointerEvent) {
  dragging.value = true
  ;(event.currentTarget as HTMLElement).setPointerCapture(event.pointerId)
  emit('drag', event.clientX)
}

function continueDrag(event: PointerEvent) {
  if (dragging.value) {
    emit('drag', event.clientX)
  }
}

function stopDrag() {
  dragging.value = false
}

function handleKeydown(event: KeyboardEvent) {
  switch (event.key) {
    case 'ArrowLeft':
      event.preventDefault()
      emit('nudge', event.shiftKey ? -10 : -2)
      break

    case 'ArrowRight':
      event.preventDefault()
      emit('nudge', event.shiftKey ? 10 : 2)
      break

    case 'Home':
      event.preventDefault()
      emit('nudge', Number.NEGATIVE_INFINITY)
      break

    case 'End':
      event.preventDefault()
      emit('nudge', Number.POSITIVE_INFINITY)
      break
  }
}
</script>

<template>
  <div
    class="resizeHandle"
    role="separator"
    tabindex="0"
    @pointerdown="startDrag"
    @pointermove="continueDrag"
    @pointerup="stopDrag"
    @pointercancel="stopDrag"
    @keydown="handleKeydown"
    @dblclick="emit('reset')" />
</template>

<style scoped>
.resizeHandle {
  position: relative;
  width: 8px;
  cursor: col-resize;
  touch-action: none;
  user-select: none;
  background: var(--ui-bg);
}

.resizeHandle::after {
  position: absolute;
  inset-block: 0;
  left: 3px;
  width: 1px;
  content: '';
  background: var(--ui-border);
}

.resizeHandle:hover::after,
.resizeHandle:focus-visible::after {
  width: 2px;
  background: var(--ui-primary);
}

.resizeHandle:focus-visible {
  outline: none;
}

@media (max-width: 700px) {
  .resizeHandle {
    display: none;
  }
}
</style>
