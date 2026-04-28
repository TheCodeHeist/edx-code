"use client";

import Editor, { useMonaco } from "@monaco-editor/react";
import { useTheme } from "next-themes";
import { CircularSpinner } from "@/components/circular-spinner";
import { useEffect } from "react";

const TextEditor = ({
  value,
  onChange,
  language,
  readOnly = false,
  lineNumbers = "on",
}: {
  value: string;
  onChange: (value: string | undefined) => void;
  language: string;
  readOnly: boolean;
  lineNumbers: "on" | "off";
}) => {
  const monaco = useMonaco();
  const theme = useTheme();

  useEffect(() => {
    if (monaco) {
      monaco.languages.register({ id: "edx" });

      monaco.languages.setMonarchTokensProvider("edx", {
        keywords: [
          "SET",
          "TO",
          "IF",
          "THEN",
          "ELSE",
          "END",
          "WHILE",
          "DO",
          "REPEAT",
          "UNTIL",
          "TIMES",
          "STEP",
          "FOR",
          "FOREACH",
          "CONTINUE",
          "BREAK",
          "FROM",
          "SEND",
          "RECEIVE",
          "READ",
          "WRITE",
          "CALL",
          "PROCEDURE",
          "FUNCTION",
          "BEGIN",
          "RETURN",
          "TRUE",
          "FALSE",
          "AND",
          "OR",
          "NOT",
        ],
        typeKeywords: ["REAL", "STRING", "BOOLEAN", "INTEGER"],
        operators: [
          "=",
          "+",
          "-",
          "*",
          "/",
          ">",
          "<",
          ">=",
          "<=",
          "<>",
          "AND",
          "OR",
          "NOT",
          "MOD",
          "DIV",
        ],
        symbols: /[=><!~?:&|+\-*\/\^%]+/,
        tokenizer: {
          root: [
            [
              /[A-Za-z_$][\w$]*/,
              {
                cases: {
                  "@typeKeywords": "keyword",
                  "@keywords": "keyword",
                  "@default": "identifier",
                },
              },
            ],
            [
              /@symbols/,
              { cases: { "@operators": "operator", "@default": "" } },
            ],
            // [/[A-Z][\w\$]*/, "type.identifier"], // to show class names nicely
            [/#.*/, "comment"],
            [/\d*\.\d+([eE][\-+]?\d+)?/, "number.float"],
            [/\d+/, "number"],
            [/'([^'\\]|\\.)*'/, "string"],
          ],
        },
      });

      monaco.languages.registerCompletionItemProvider("edx", {
        provideCompletionItems: (model, position) => {
          const word = model.getWordUntilPosition(position);
          const range = {
            startLineNumber: position.lineNumber,
            endLineNumber: position.lineNumber,
            startColumn: word.startColumn,
            endColumn: word.endColumn,
          };

          const suggestions = [
            {
              label: "SET",
              kind: monaco.languages.CompletionItemKind.Snippet,
              insertText: "SET ${1:identifier} TO ${0:value}",
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              range,
            },
            {
              label: "SEND",
              kind: monaco.languages.CompletionItemKind.Snippet,
              insertText: "SEND ${1:message} TO ${2:device}",
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              range,
            },
            {
              label: "SEND-TO-DISPLAY",
              kind: monaco.languages.CompletionItemKind.Snippet,
              insertText: "SEND ${1:message} TO DISPLAY",
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              range,
            },
            {
              label: "RECEIVE",
              kind: monaco.languages.CompletionItemKind.Snippet,
              insertText:
                "RECEIVE ${1:identifier} FROM (${2:type}) ${0:source}",
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              range,
            },
            {
              label: "RECEIVE-FROM-KEYBOARD",
              kind: monaco.languages.CompletionItemKind.Snippet,
              insertText: "RECEIVE ${1:identifier} FROM (STRING) KEYBOARD",
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              range,
            },
            {
              label: "RECEIVE-INTEGER-FROM-KEYBOARD",
              kind: monaco.languages.CompletionItemKind.Snippet,
              insertText: "RECEIVE ${1:identifier} FROM (INTEGER) KEYBOARD",
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              range,
            },
            {
              label: "RECEIVE-REAL-FROM-KEYBOARD",
              kind: monaco.languages.CompletionItemKind.Snippet,
              insertText: "RECEIVE ${1:identifier} FROM (REAL) KEYBOARD",
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              range,
            },
            {
              label: "RECEIVE-BOOLEAN-FROM-KEYBOARD",
              kind: monaco.languages.CompletionItemKind.Snippet,
              insertText: "RECEIVE ${1:identifier} FROM (BOOLEAN) KEYBOARD",
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              range,
            },
            {
              label: "REPEAT",
              kind: monaco.languages.CompletionItemKind.Snippet,
              insertText: [
                "REPEAT ${1:times} TIMES",
                "BEGIN",
                "\t${0:statements}",
                "END REPEAT",
              ].join("\n"),
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              range,
            },
            {
              label: "IF",
              kind: monaco.languages.CompletionItemKind.Snippet,
              insertText: [
                "IF ${1:condition} THEN",
                "\t${0:statements}",
                "END IF",
              ].join("\n"),
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              range,
            },
            {
              label: "IF-ELSE",
              kind: monaco.languages.CompletionItemKind.Snippet,
              insertText: [
                "IF ${1:condition} THEN",
                "\t${2:statements}",
                "ELSE",
                "\t${0:else_statements}",
                "END IF",
              ].join("\n"),
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              range,
            },
            {
              label: "WHILE",
              kind: monaco.languages.CompletionItemKind.Snippet,
              insertText: [
                "WHILE ${1:condition} DO",
                "\t${0:statements}",
                "END WHILE",
              ].join("\n"),
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              range,
            },
            {
              label: "FOR",
              kind: monaco.languages.CompletionItemKind.Snippet,
              insertText: [
                "FOR ${1:variable} FROM ${2:start} TO ${3:end} DO",
                "\t${0:statements}",
                "END FOR",
              ].join("\n"),
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              range,
            },
            {
              label: "PROCEDURE",
              kind: monaco.languages.CompletionItemKind.Snippet,
              insertText: [
                "PROCEDURE ${1:name}(${2:parameters})",
                "BEGIN",
                "\t${0:statements}",
                "END PROCEDURE",
              ].join("\n"),
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              range,
            },
            {
              label: "FUNCTION",
              kind: monaco.languages.CompletionItemKind.Snippet,
              insertText: [
                "FUNCTION ${1:name}(${2:parameters}) RETURNS ${3:type}",
                "BEGIN",
                "\t${0:statements}",
                "END FUNCTION",
              ].join("\n"),
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              range,
            },
          ];

          return { suggestions };
        },
      });

      // monaco.editor.defineTheme("edx", {
      //   base: "vs",
      //   inherit: false,
      //   rules: [
      //     { token: "comment", foreground: "6A9955" },
      //     { token: "number", foreground: "B5CEA8" },
      //     { token: "keyword", foreground: "569CD6", fontStyle: "bold" },
      //     { token: "string", foreground: "CE9178" },
      //   ],
      //   colors: {
      //     "editor.background": "#1E1E1E",
      //   },
      // });
    }
  }, [monaco, theme]);

  return (
    <Editor
      language={language}
      value={value}
      onChange={onChange}
      loading={<CircularSpinner />}
      theme="vs-dark"
      options={{
        autoClosingBrackets: "always",
        autoClosingQuotes: "always",
        formatOnType: true,
        formatOnPaste: true,
        trimAutoWhitespace: true,
        readOnly: readOnly,
        lineNumbers: lineNumbers,
        minimap: { enabled: false },
        wordWrap: "on",
      }}
    />
  );
};

export default TextEditor;
