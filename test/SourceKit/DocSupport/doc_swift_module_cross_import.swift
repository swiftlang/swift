// RUN: %empty-directory(%t)

// Check the interface shows the decls from each of A's cross-import overlays.
//
// RUN: %sourcekitd-test -req=doc-info -module A -- -I %S/../Inputs/CrossImport > %t.response
// RUN: diff --strip-trailing-cr -u %s.response %t.response

