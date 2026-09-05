func demo() -> Int { return 42 }

// This test ensures sourcekitd accepts frontend arguments (a compiler-args
// list beginning with `-frontend`); the request is sent via
// -json-request-path rather than the natural because that form has
// sourcekitd-test prepend `-module-cache-path` to the request's compiler args,
// which would incorrectly fail this test

// RUN: echo '{ key.request: source.request.cursorinfo, key.sourcefile: "%s", key.offset: 5, key.compilerargs: [ "-frontend", "-module-name", "Test", "%s" ] }' > %t.json
// RUN: %sourcekitd-test -json-request-path %t.json | %FileCheck %s

// The module name `Test` is set only through the frontend args, so seeing it
// baked into the USR (`4Test`) proves the frontend path parsed and applied them.
// CHECK: key.kind: source.lang.swift.decl.function.free
// CHECK: key.usr: "s:4Test4demoSiyF"
