import * as monaco from 'monaco-editor'

export const MONKE_LANGUAGE_ID = 'monke'

let registered = false

const builtins = {
  print: 'print(...values)\n\nWrites each value to standard output.',
  len: 'len(value)\n\nReturns the length of a string or array.',
  first: 'first(array)\n\nReturns the first element, or null.',
  last: 'last(array)\n\nReturns the last element, or null.',
  push: 'push(array, value)\n\nReturns a new array containing value.',
}

export function registerMonkeLanguage() {
  if (registered) return
  registered = true

  monaco.languages.register({
    id: MONKE_LANGUAGE_ID,
    extensions: ['.monke'],
    aliases: ['Monke', 'monke'],
  })

  monaco.languages.setLanguageConfiguration(MONKE_LANGUAGE_ID, {
    brackets: [
      ['{', '}'],
      ['[', ']'],
      ['(', ')'],
    ],
    autoClosingPairs: [
      { open: '{', close: '}' },
      { open: '[', close: ']' },
      { open: '(', close: ')' },
      { open: '"', close: '"' },
    ],
    surroundingPairs: [
      { open: '{', close: '}' },
      { open: '[', close: ']' },
      { open: '(', close: ')' },
      { open: '"', close: '"' },
    ],
  })

  monaco.languages.setMonarchTokensProvider(MONKE_LANGUAGE_ID, {
    defaultToken: 'invalid',

    keywords: ['function', 'let', 'if', 'else', 'return', 'true', 'false', 'for'],

    builtins: Object.keys(builtins),

    tokenizer: {
      root: [
        [/\s+/, 'white'],

        [
          /[a-zA-Z_]+/,
          {
            cases: {
              '@keywords': 'keyword',
              '@builtins': 'predefined',
              '@default': 'identifier',
            },
          },
        ],

        [/\d+/, 'number'],
        [/"[^"]*"/, 'string'],
        [/"[^"]*$/, 'string.invalid'],

        [/(?:\+=|-=|\*=|\/=|==|!=|[=+\-*/!<>])/, 'operator'],
        [/[{}()[\]]/, '@brackets'],
        [/[;,:]/, 'delimiter'],
      ],
    },
  })

  monaco.languages.registerCompletionItemProvider(MONKE_LANGUAGE_ID, {
    provideCompletionItems(model, position) {
      const word = model.getWordUntilPosition(position)
      const range = {
        startLineNumber: position.lineNumber,
        endLineNumber: position.lineNumber,
        startColumn: word.startColumn,
        endColumn: word.endColumn,
      }

      return {
        suggestions: [
          {
            label: 'print',
            kind: monaco.languages.CompletionItemKind.Function,
            insertText: 'print(${1:value});',
            insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
            detail: 'Print values to standard output',
            range,
          },
          {
            label: 'len',
            kind: monaco.languages.CompletionItemKind.Function,
            insertText: 'len(${1:value})',
            insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
            detail: 'Get the length of a string or array',
            range,
          },
          {
            label: 'first',
            kind: monaco.languages.CompletionItemKind.Function,
            insertText: 'first(${1:array})',
            insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
            range,
          },
          {
            label: 'last',
            kind: monaco.languages.CompletionItemKind.Function,
            insertText: 'last(${1:array})',
            insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
            range,
          },
          {
            label: 'push',
            kind: monaco.languages.CompletionItemKind.Function,
            insertText: 'push(${1:array}, ${2:value})',
            insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
            range,
          },
          {
            label: 'function',
            kind: monaco.languages.CompletionItemKind.Snippet,
            insertText: 'function(${1:parameters}) {\n\t${2}\n}',
            insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
            detail: 'Function expression',
            range,
          },
        ],
      }
    },
  })

  monaco.languages.registerHoverProvider(MONKE_LANGUAGE_ID, {
    provideHover(model, position) {
      const word = model.getWordAtPosition(position)

      if (!word || !(word.word in builtins)) {
        return null
      }

      return {
        contents: [
          {
            value: `\`\`\`monke\n${builtins[word.word as keyof typeof builtins]}\n\`\`\``,
          },
        ],
      }
    },
  })
}
