export const compilerWorker = new Worker(new URL('./core/worker_compiler.js', import.meta.url), {
  type: 'module',
})
