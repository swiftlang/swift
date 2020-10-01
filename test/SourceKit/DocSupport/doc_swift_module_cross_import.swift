// RUN: %empty-directory(%t.mod)
// RUN: %empty-directory(%t.mod/mcp)

// Check doc info shows the decls from each of A's cross-import overlays and lists the required bystander modules.
//
// RUN: %sourcekitd-test -req=doc-info -module A -- -I %S/../Inputs/CrossImport -module-cache-path %t.mod/mcp > %t.response
// RUN: %diff -u %s.A.response %t.response

// Set up a cross-import module with doc comments and check the synthesized comments don't appear in the fully_annotated_decl entries.
//
// RUN: %target-swift-frontend -emit-module-path %t.mod/_OtherCAdditions.swiftmodule -emit-module-doc-path %t.mod/_OtherCAdditions.swiftdoc -module-cache-path %t.mod/mcp -I %S/../Inputs/CrossImport %S/../Inputs/CrossImport/_OtherCAdditions.swift -parse-as-library -enable-cross-import-overlays
// RUN: %sourcekitd-test -req=doc-info -module Other -- -target %target-triple -I %S/../Inputs/CrossImport -I %t.mod/ -module-cache-path %t.mod/mcp > %t.response
// RUN: %diff -u %s.Other.response %t.response

