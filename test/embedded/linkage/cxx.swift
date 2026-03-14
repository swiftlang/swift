// Check that C++ functions are exported as needed.

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_DeferredCodeGen

// RUN: %target-swift-frontend -emit-ir -o - %s -I %S/Inputs -package-name MyPackage -enable-experimental-feature Embedded -enable-experimental-feature DeferredCodeGen -parse-as-library -cxx-interoperability-mode=default | %FileCheck %s

// CHECK: define hidden swiftcc void @"$e3cxx2f1yyF"()
@_expose(Cxx)
func f1() { }

// CHECK-NOT: 2f2
@_expose(!Cxx)
public func f2() { }
