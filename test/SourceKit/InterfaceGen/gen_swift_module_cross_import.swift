// RUN: %empty-directory(%t.mod)
// RUN: %empty-directory(%t.mod/mcp)
// RUN: %sourcekitd-test -req=interface-gen -module A -- -I %S/Inputs/CrossImport > %t.response
// RUN: diff --strip-trailing-cr -u %s.response %t.response

