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
        autoClosingBrackets: "never",
        autoClosingQuotes: "never",
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
