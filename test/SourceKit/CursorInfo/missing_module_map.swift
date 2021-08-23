// RUN: %empty-directory(%t.mcp)
// RUN: %sourcekitd-test -req=cursor -pos=6:9 %s -- -Xcc -fmodule-map-file=/some/missing/file -module-cache-path %t.mcp %s | %FileCheck %s

// We used to fail to load the stdlib if there is a `-fmodule-map-file` option pointing to a missing file

let x: String = "abc"
// CHECK: source.lang.swift.ref.struct ()
// CHECK: String
// CHECK: s:SS
