import { onMounted, onUnmounted, type MaybeRef, toValue } from 'vue'

/**
 * Calls `cb` when all keys in a combination are held, such as ['Control', 'Enter'].
 * Keys use KeyboardEvent.key names and are matched case-insensitively.
 */
export function useHotKeys(keys: MaybeRef<string[]>, cb: (e: KeyboardEvent) => void) {
  const pressedKeys = new Map<string, string>()

  const handleKeyDown = (e: KeyboardEvent) => {
    const key = e.key.toLowerCase()
    pressedKeys.set(e.code || key, key)

    if (e.isComposing) return

    const combination = toValue(keys).map(key => key.toLowerCase())
    const activeKeys = new Set(pressedKeys.values())

    // Modifier flags also work when a modifier was pressed before mounting.
    for (const [modifier, active] of [
      ['control', e.ctrlKey],
      ['alt', e.altKey],
      ['shift', e.shiftKey],
      ['meta', e.metaKey],
    ] as const) {
      if (active) activeKeys.add(modifier)
      else activeKeys.delete(modifier)
    }

    if (combination.includes(key) && combination.every(key => activeKeys.has(key))) {
      cb(e)
    }
  }

  const handleKeyUp = (e: KeyboardEvent) => {
    pressedKeys.delete(e.code || e.key.toLowerCase())
  }

  const clearPressedKeys = () => {
    pressedKeys.clear()
  }

  onMounted(() => {
    window.addEventListener('keydown', handleKeyDown)
    window.addEventListener('keyup', handleKeyUp)
    window.addEventListener('blur', clearPressedKeys)
  })

  onUnmounted(() => {
    window.removeEventListener('keydown', handleKeyDown)
    window.removeEventListener('keyup', handleKeyUp)
    window.removeEventListener('blur', clearPressedKeys)
    clearPressedKeys()
  })
}
