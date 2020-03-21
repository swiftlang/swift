// RUN: %empty-directory(%t.mod)
// RUN: %empty-directory(%t.mod/mcp)

// Check the interface shows the decls from each of A's cross-import overlays.
//
// RUN: %sourcekitd-test -req=interface-gen -module A -- -I %S/../Inputs/CrossImport > %t.response
// RUN: diff --strip-trailing-cr -u %s.response %t.response

// Make sure cursor info within the generated interface of A on one of the
// decls originally from a cross-import decls shows 'A' as the parent module.
//
// RUN: %sourcekitd-test -req=interface-gen-open -module A -- -I %S/../Inputs/CrossImport == -req=cursor -print-raw-response -pos=11:15 -- -I %S/../Inputs/CrossImport -Xfrontend -enable-cross-import-overlays > %t.response
// RUN: %FileCheck --input-file %t.response %s
//
// CHECK: key.name: "From_ABAdditionsType"
// CHECK: key.modulename: "A"

