// This test verifies that command line parsing restricts use of features that
// are not allowed non-production compilers.

// REQUIRES: no_asserts
// REQUIRES: swift_feature_AccessLevelOnImport

// 'AccessLevelOnImport' is allowed in production
// RUN: %target-swift-frontend -typecheck %s -enable-experimental-feature AccessLevelOnImport -verify

// 'ParserValidation' is NOT allowed in production
// RUN: not %target-swift-frontend -typecheck %s -enable-experimental-feature ParserValidation 2>&1 | %FileCheck %s

// CHECK: <unknown>:0: error: experimental feature 'ParserValidation' cannot be enabled in production compiler

import Swift
