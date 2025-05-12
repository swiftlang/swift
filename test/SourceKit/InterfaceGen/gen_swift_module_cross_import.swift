// RUN: %empty-directory(%t.mod)
// RUN: %empty-directory(%t.mod/mcp)

// Check the interface shows the decls from each of A's cross-import overlays.
//
// RUN: %sourcekitd-test -req=interface-gen -module A -- -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import  -I %S/../Inputs/CrossImport -module-cache-path %t.mod/mcp > %t.response
// RUN: %diff -u %s.A.response %t.response

// Make sure cursor info within the generated interface of A on one of the
// decls originally from a cross-import decls shows 'A' as the parent module.
//
// RUN: %sourcekitd-test -req=interface-gen-open -module A -- -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import  -I %S/../Inputs/CrossImport -module-cache-path %t.mod/mcp == -req=cursor -print-raw-response -pos=9:15 -- -I %S/../Inputs/CrossImport -Xfrontend -enable-cross-import-overlays > %t.response
// RUN: %FileCheck --input-file %t.response %s
//
// CHECK: key.name: "From_ABAdditionsType"
// CHECK: key.modulename: "A"

// Set up a cross-import module with doc comments that also uses cross-imports.
// This lets us check that imports of overlays are printed as their
// underlying modules, and that the synthesized comment above each decl
// originally from an overlay appears before any doc comments.
//
// RUN: %target-swift-frontend -emit-module-path %t.mod/_OtherCAdditions.swiftmodule -emit-module-doc-path %t.mod/_OtherCAdditions.swiftdoc -module-cache-path %t.mod/mcp -I %S/../Inputs/CrossImport %S/../Inputs/CrossImport/_OtherCAdditions.swift -parse-as-library -enable-cross-import-overlays -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import
// RUN: %sourcekitd-test -req=interface-gen -module Other -- -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import  -target %target-triple -I %S/../Inputs/CrossImport -I %t.mod/ -module-cache-path %t.mod/mcp > %t.response
// RUN: %diff -u %s.Other.response %t.response
