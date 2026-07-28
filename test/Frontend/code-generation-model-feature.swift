// Errors when CodeGenerationModel is used outside of Embedded Swift.
// RUN: not %target-swift-frontend -typecheck %s -enable-experimental-feature CodeGenerationModel=interface 2>&1 | %FileCheck %s --check-prefix=NOT-EMBEDDED
// RUN: not %target-swift-frontend -typecheck %s -enable-experimental-feature CodeGenerationModel=implementation 2>&1 | %FileCheck %s --check-prefix=NOT-EMBEDDED
// RUN: not %target-swift-frontend -typecheck %s -enable-experimental-feature CodeGenerationModel=inlinable 2>&1 | %FileCheck %s --check-prefix=NOT-EMBEDDED

// NOT-EMBEDDED: error: experimental feature 'CodeGenerationModel' can only be used in Embedded Swift mode

// Errors on an unrecognized value, even in Embedded mode.
// RUN: not %target-swift-frontend -typecheck %s -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=foo 2>&1 | %FileCheck %s --check-prefix=BAD-VALUE

// BAD-VALUE: error: invalid value 'foo' for experimental feature 'CodeGenerationModel'; expected 'interface', 'implementation', or 'inlinable'

// Accepted values typecheck cleanly in Embedded mode.
// RUN: %target-swift-frontend -typecheck -parse-stdlib %s -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface
// RUN: %target-swift-frontend -typecheck -parse-stdlib %s -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation
// RUN: %target-swift-frontend -typecheck -parse-stdlib %s -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=inlinable

// REQUIRES: swift_feature_Embedded
