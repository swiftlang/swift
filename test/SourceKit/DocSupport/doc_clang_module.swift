// REQUIRES: objc_interop
// RUN: %sourcekitd-test -req=doc-info -module Foo -- -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt %clang-importer-sdk | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response
