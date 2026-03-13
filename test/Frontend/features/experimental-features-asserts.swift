// This test verifies that command line parsing allows use of any features with
// an asserts compilers.

// REQUIRES: asserts
// REQUIRES: swift_feature_AccessLevelOnImport
// REQUIRES: swift_feature_ParserValidation

// 'AccessLevelOnImport' is allowed in production
// RUN: %target-swift-frontend -typecheck %s -enable-experimental-feature AccessLevelOnImport -verify

// 'ParserValidation' is NOT allowed in production, but we are building with an asserts compiler.
// RUN: %target-swift-frontend -typecheck %s -enable-experimental-feature ParserValidation

import Swift
