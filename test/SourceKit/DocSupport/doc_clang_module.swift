// SWIFT_ENABLE_TENSORFLOW
//
// NOTE: This test is disabled on the 'tensorflow' branch because we are
// actively developing and changing attributes such as `@differentiable` and
// `@compilerEvaluable`. When various features get merged to 'master', a
// canonical update of these tests will be included.
//
// UNSUPPORTED: tensorflow

// REQUIRES: objc_interop

// FIXME: the test output we're comparing to is specific to macOS.
// REQUIRES-ANY: OS=macosx

// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays

// RUN: %sourcekitd-test -req=doc-info -module Foo -- -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response
