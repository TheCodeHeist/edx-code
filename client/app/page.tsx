"use client";

import Navbar from "@/components/navbar";
import { Separator } from "@/components/ui/separator";
import { Card, CardHeader } from "@/components/ui/card";
import { useState } from "react";
import TextEditor from "@/components/editor";
import { Button } from "@/components/ui/button";
import { PlayIcon } from "@phosphor-icons/react";
import { to_python } from "@/pkg/edx_code.js";

export default function Home() {
  // Handle value and onChange for the editor
  const [editorValue, setEditorValue] = useState<string>(
    "# Welcome to the Pearson Edexcel Pseudocode Playground!\n\n# You can write your EDX code here and click the play button to see the Python equivalent.\n\n# Example:\n# SET x TO 10\n# SEND x * 2 TO DISPLAY\n",
  );
  const [outputValue, setOutputValue] = useState<string>("");
  const [consoleOutput, setConsoleOutput] = useState<string>("");

  const handleTranspilation = async () => {
    // Transpile the code using to_python
    try {
      const pythonCode = to_python(editorValue);
      setOutputValue(pythonCode);
      setConsoleOutput("Transpilation successful!");
    } catch (error) {
      setConsoleOutput(`${error}`);
    }
  };

  return (
    <div className="h-screen">
      <Navbar />
      <Separator className="mb-2" />

      <div className="flex-1 grid grid-cols-2 grid-rows-2 gap-4 p-4">
        <Card className="h-[calc(70vh)] w-full overflow-hidden">
          <CardHeader className="flex items-center justify-between">
            <p className="text-xl">EDX Code Editor</p>

            <Button onClick={handleTranspilation}>
              <PlayIcon className="size-4" />
              <span className="sr-only">Run code</span>
            </Button>
          </CardHeader>

          <TextEditor
            value={editorValue}
            onChange={(v) => setEditorValue(v || "")}
            language="edx"
            readOnly={false}
            lineNumbers="on"
          />
        </Card>

        <Card className="h-[calc(70vh)] w-full overflow-hidden">
          <CardHeader>
            <p className="text-xl">Python Output</p>
          </CardHeader>

          <TextEditor
            value={outputValue}
            onChange={(v) => setOutputValue(v || "")}
            language="python"
            readOnly
            lineNumbers="on"
          />
        </Card>

        <Card className="h-[calc(20vh)] w-full overflow-hidden col-span-2">
          <CardHeader>
            <p className="text-xl">Console</p>
          </CardHeader>

          <TextEditor
            value={consoleOutput}
            onChange={(v) => setConsoleOutput(v || "")}
            language="shell"
            readOnly
            lineNumbers="off"
          />
        </Card>
      </div>
    </div>
  );
}
