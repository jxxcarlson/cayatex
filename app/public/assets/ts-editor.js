// @ts-nocheck
export function register() {
  customElements.define(
    "ts-editor",
    class extends HTMLElement {
      constructor() {
        super();
        // debugger;
        // console.log(this);
        this._editorValue = this.getAttribute("editorValue");
        this._dts = this.getAttribute("dts");
        //   "-- If you see this, the Elm code didn't set the value.";
      }

      get editorValue() {
        return this._editorValue;
      }

      get dts() {
        return this._dts;
      }

      set dts(value) {
        if (this._dts === value) return;
        this._dts = value;
      }

      set editorValue(value) {
        if (this._editorValue === value) return;
        this._editorValue = value;
        // if (!this._editor) return;
        // this._editor.setValue(value);
      }

      connectedCallback() {
        this.innerHTML = `<div id="loader">Loading...</div>
<div id="monaco-editor-embed" style="width: 100%;" />`;
        // First set up the VSCode loader in a script tag
        const getLoaderScript = document.createElement("script");
        getLoaderScript.src = "https://www.typescriptlang.org/js/vs.loader.js";
        getLoaderScript.async = true;
        getLoaderScript.onload = () => {
          // Now the loader is ready, tell require where it can get the version of monaco, and the sandbox
          // This version uses the latest version of the sandbox, which is used on the TypeScript website

          // For the monaco version you can use unpkg or the TypeSCript web infra CDN
          // You can see the available releases for TypeScript here:
          // https://typescript.azureedge.net/indexes/releases.json
          //
          require.config({
            paths: {
              vs: "https://typescript.azureedge.net/cdn/4.1.3/monaco/min/vs",
              // vs: 'https://unpkg.com/@typescript-deploys/monaco-editor@4.0.5/min/vs',
              sandbox: "https://www.typescriptlang.org/js/sandbox",
            },
            // This is something you need for monaco to work
            ignoreDuplicateModules: ["vs/editor/editor.main"],
          });

          // Grab a copy of monaco, TypeScript and the sandbox
          require([
            "vs/editor/editor.main",
            "vs/language/typescript/tsWorker",
            "sandbox/index",
          ], (main, _tsWorker, sandboxFactory) => {
            const isOK = main && window.ts && sandboxFactory;
            if (isOK) {
              document
                .getElementById("loader")
                .parentNode.removeChild(document.getElementById("loader"));
            } else {
              console.error(
                "Could not get all the dependencies of sandbox set up!"
              );
              console.error(
                "main",
                !!main,
                "ts",
                !!window.ts,
                "sandbox",
                !!sandbox
              );
              return;
            }

            // Create a sandbox and embed it into the the div #monaco-editor-embed
            const sandboxConfig = {
              text: this._editorValue,
              compilerOptions: {},
              domID: "monaco-editor-embed",
            };

            const sandboxEditor = sandboxFactory.createTypeScriptSandbox(
              sandboxConfig,
              main,
              window.ts
            );
            sandboxEditor.monaco.editor.setTheme("vs-dark");
            sandboxEditor.editor.onDidChangeModelContent(() => {
              const code = sandboxEditor.editor.getModel().getValue();
              this._editorValue = code;
              this.dispatchEvent(
                new CustomEvent("editorChanged", {
                  // bubbles: true,
                  // composed: true,
                  // detail: { content: code },
                })
              );
            });
            sandboxEditor.editor.onDidChangeModelDecorations(() => {
              // source: https://github.com/microsoft/monaco-editor/issues/794#issuecomment-583367666
              updateEditorHeight(); // typing
              requestAnimationFrame(updateEditorHeight); // folding
            });
            let prevHeight = 0;

            const updateEditorHeight = () => {
              const editorElement = sandboxEditor.editor.getDomNode();

              if (!editorElement) {
                return;
              }

              const lineHeight = sandboxEditor.editor.getOption(
                sandboxEditor.monaco.editor.EditorOption.lineHeight
              );
              const lineCount =
                sandboxEditor.editor.getModel()?.getLineCount() || 1;
              // const height = 500;

              const height = Math.max(
                sandboxEditor.editor.getTopForLineNumber(lineCount + 1) +
                  lineHeight,
                500
              );

              if (prevHeight !== height) {
                prevHeight = height;
                editorElement.style.height = `${height}px`;
                sandboxEditor.editor.layout();
              }
            };
            updateEditorHeight();

            if (this._dts) {
              sandboxEditor.monaco.languages.typescript.typescriptDefaults.addExtraLib(
                this._dts,
                "file:///elm.d.ts"
              );
            }
          });
        };

        document.body.appendChild(getLoaderScript);
      }
    }
  );
}