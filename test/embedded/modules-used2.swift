// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -enable-experimental-feature SymbolLinkageMarkers -enable-experimental-feature Embedded -parse-as-library -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift
// RUN: %target-swift-frontend -enable-experimental-feature SymbolLinkageMarkers -enable-experimental-feature Embedded -parse-as-library -I %t %t/Main.swift -emit-sil | %FileCheck %s --check-prefix CHECK-SIL
// RUN: %target-swift-frontend -enable-experimental-feature SymbolLinkageMarkers -enable-experimental-feature Embedded -parse-as-library -I %t %t/Main.swift -c -o %t/a.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/a.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_SymbolLinkageMarkers

// BEGIN MyModule.swift

@_used
@_cdecl("main")
func main() -> CInt {
	print("main in a submodule")
	return 0
}

@_used
func foo() {
}

// BEGIN Main.swift

import MyModule

// CHECK-SIL:      // main()
// CHECK-SIL-NEXT: sil @$e8MyModule4mains5Int32VyF : $@convention(thin) () -> Int32 {
// CHECK-SIL: 	   // main
// CHECK-SIL-NEXT: sil [thunk] @main : $@convention(c) () -> Int32
// CHECK-SIL:      // foo()
// CHECK-SIL-NEXT: sil @$e8MyModule3fooyyF : $@convention(thin) () -> () {

// CHECK: main in a submodule
