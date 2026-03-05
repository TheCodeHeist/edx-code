import { useTheme } from "next-themes";
import * as React from "react";
import { useEffect, useState } from "react";
import { Button } from "@/components/ui/button";
import { MoonIcon, SunIcon } from "@phosphor-icons/react";

export function ThemeToggle() {
  const { theme, setTheme, resolvedTheme } = useTheme();
  const [, startTransition] = React.useTransition();
  const [mounted, setMounted] = useState(false);

  useEffect(() => {
    // eslint-disable-next-line react-hooks/set-state-in-effect
    setMounted(true);
  }, []);

  useEffect(() => {
    if (!resolvedTheme) return;
    document.documentElement.setAttribute("data-theme", resolvedTheme);
  }, [resolvedTheme]);

  if (!mounted) return null; // Prevent server-side rendering

  const isDarkMode = theme === "dark" || resolvedTheme === "dark";

  return (
    <Button
      variant="ghost"
      size="icon"
      onClick={() => {
        startTransition(() => {
          setTheme(isDarkMode ? "light" : "dark");
        });
      }}
    >
      {isDarkMode ? (
        <MoonIcon className="transition-all size-5" />
      ) : (
        <SunIcon className="transition-all size-5" />
      )}
      <span className="sr-only">Toggle theme</span>
    </Button>
  );
}
