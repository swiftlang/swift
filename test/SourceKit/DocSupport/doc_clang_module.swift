// REQUIRES: objc_interop

// FIXME: the test output we're comparing to is specific to macOS.
// REQUIRES: OS=macosx

// RUN: %sourcekitd-test -req=doc-info -module Foo -- -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt -target %target-triple %clang-importer-sdk | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response
