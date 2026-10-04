// A function with an opaque result type is not generic, so it is strongly
// defined like any other @export(interface) function.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -o %t/Lib.ll %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -validate-tbd-against-ir=all
// RUN: %FileCheck -check-prefix IR %s < %t/Lib.ll
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -validate-tbd-against-ir=all -O

// REQUIRES: swift_feature_Embedded

public protocol P {
  func f() -> Int
}

extension Int: P {
  public func f() -> Int { self }
}

// IR: define {{(protected |dllexport )?}}swiftcc void @"$e3Lib6opaqueQryF"(
public func opaque() -> some P { 42 }
