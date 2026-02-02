// Check that C functions are exported as needed.

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_DeferredCodeGen

// RUN: %target-swift-frontend -emit-ir -o - %s -I %S/Inputs -package-name MyPackage -enable-experimental-feature Embedded -enable-experimental-feature DeferredCodeGen -parse-as-library | %FileCheck %s

import MyModuleExports

// ---------------------------------------------------------------------------
// @c / @_cdecl on > internal declarations cause symbol emission
// ---------------------------------------------------------------------------

// CHECK: define {{(protected |dllexport )?}}void @lib_publicCDeclFunc()
@_cdecl("lib_publicCDeclFunc")
public func f1() { }

// CHECK: define {{(protected |dllexport )?}}void @lib_publicCFunc()
@c(lib_publicCFunc)
package func f2() { }

// CHECK: define hidden void @lib_publicCDeclFunc3
@_cdecl("lib_publicCDeclFunc3")
internal func f3() { }

// CHECK: define hidden void @lib_publicCDeclFunc4
@c(lib_publicCDeclFunc4)
internal func f4() { }

// CHECK-NOT: lib_publicCDeclFunc5
@_cdecl("lib_publicCDeclFunc5")
private func f5() { }

// CHECK-NOT: lib_publicCDeclFunc6
@c(lib_publicCDeclFunc6)
private func f6() { }

// ---------------------------------------------------------------------------
// @implementation @c / @_cdecl cause symbol emission
// ---------------------------------------------------------------------------

// CHECK: define {{(protected |dllexport )?}}double @clib_func1
@_cdecl("clib_func1") @implementation
public func clib_func1() -> Double { 0 }

// CHECK: define {{(protected |dllexport )?}}double @clib_func2
@c @implementation
package func clib_func2() -> Double { 0 }

// CHECK: define hidden double @clib_func3
@_cdecl("clib_func3") @implementation
internal func clib_func3() -> Double { 0 }

// CHECK: define hidden double @clib_func4
@c @implementation
internal func clib_func4() -> Double { 0 }

// ---------------------------------------------------------------------------
// @section causes symbol emission
// ---------------------------------------------------------------------------

// CHECK: define {{(protected |dllexport )?}}swiftcc void @"$e1c15symbolInSectionyyF"
@section("__TEXT,__mysection")
public func symbolInSection() { }

// CHECK: define hidden swiftcc void @"$e1c23internalSymbolInSectionyyF"
@section("__TEXT,__mysection")
func internalSymbolInSection() { }

// ---------------------------------------------------------------------------
// @used causes symbol emission
// ---------------------------------------------------------------------------

// CHECK: define hidden swiftcc void @"$e1c10usedSymbolyyF"
@used
func usedSymbol() { }
