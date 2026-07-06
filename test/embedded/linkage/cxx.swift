// Check that C++ functions are exported as needed.

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// UNSUPPORTED: CPU=wasm32

// RUN: %target-swift-frontend -emit-ir -o - %s -I %S/Inputs -package-name MyPackage -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -parse-as-library -cxx-interoperability-mode=default | %FileCheck %s

// CHECK: define {{(linkonce_odr )?}}hidden swiftcc void @"$e3cxx2f1yyF"()
@_expose(Cxx)
func f1() { }

// CHECK: define {{(linkonce_odr )?(protected |dllexport )?}}swiftcc void @"$e3cxx2f2yyF"()
@_expose(!Cxx)
public func f2() { }
