// Under the "inlinable" code generation model, which symbols are strongly
// defined depends on how they are used, so there can be no TBD file.

// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -typecheck %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -typecheck %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=inlinable -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib 2>&1 | %FileCheck %s

// The other code generation models can emit a TBD file, as can non-Embedded
// Swift.
// RUN: %target-swift-frontend -typecheck %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib
// RUN: %target-swift-frontend -typecheck %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib
// RUN: %target-swift-frontend -typecheck %s -parse-as-library -module-name Lib -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib

// REQUIRES: swift_feature_Embedded
// REQUIRES: VENDOR=apple

// CHECK: error: cannot emit a TBD file with the 'inlinable' code generation model of Embedded Swift; use '-enable-experimental-feature CodeGenerationModel=interface' or '-enable-experimental-feature CodeGenerationModel=implementation'

public func f() {}
