// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: %sourcekitd-test \
// RUN: -json-request-path %t/1-open-without-sourcefile.json \
// RUN: == -json-request-path %t/2-close.json \
// RUN: == -json-request-path %t/3-reopen-without-compiler-args.json \
// RUN: == -json-request-path %t/4-cursor-info.json

// This used to crash with a nullptr dereference because we didn't store a 
// snapshot in the FileContents of a.swift since it wasn't opened with a 
// key.sourcefile argument.

// BEGIN 1-open-without-sourcefile.json
{
  key.request: source.request.editor.open,
  key.name: "/invalid/a.swift",
  key.compilerargs: [
    "/invalid/a.swift",
    "/invalid/b.swift"
  ],
  key.sourcetext: "",
  key.enablesyntaxmap: 0,
  key.enablesubstructure: 0,
  key.enablediagnostics: 0
}
// BEGIN 2-close.json
{
  key.request: source.request.editor.close,
  key.name: "/invalid/a.swift"
}
// BEGIN 3-reopen-without-compiler-args.json
{
  key.request: source.request.editor.open,
  key.name: "/invalid/a.swift",
  key.compilerargs: [
  ],
  key.sourcetext: "",
  key.enablesyntaxmap: 0,
  key.enablesubstructure: 0,
  key.enablediagnostics: 0
}
// BEGIN 4-cursor-info.json
{
  key.request: source.request.cursorinfo,
  key.compilerargs: [
    "/invalid/a.swift",
    "/invalid/b.swift"
  ],
  key.offset: 0,
  key.sourcefile: "/invalid/a.swift"
}
