// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -enable-experimental-feature SymbolLinkageMarkers -enable-experimental-feature Embedded -parse-as-library -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift
// RUN: %target-swift-frontend -enable-experimental-feature SymbolLinkageMarkers -enable-experimental-feature Embedded -parse-as-library -I %t %t/Main.swift -emit-sil | %FileCheck %s --check-prefix CHECK-SIL
// RUN: %target-swift-frontend -enable-experimental-feature SymbolLinkageMarkers -enable-experimental-feature Embedded -parse-as-library -I %t %t/Main.swift -c -o %t/a.o
// RUN: %target-clang %t/a.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_SymbolLinkageMarkers

// BEGIN MyModule.swift

@_used
@_section("__DATA,__mysection")
let i_am_not_referenced = 42

// BEGIN Main.swift

import MyModule

@_silgen_name(raw: "section$start$__DATA$__mysection")
var mysection_start: Int

@_silgen_name(raw: "section$end$__DATA$__mysection")
var mysection_end: Int

@main
struct Main {
  static func main() {
    let start = UnsafeRawPointer(&mysection_start)
    let end = UnsafeRawPointer(&mysection_end)
    let size = end - start
    let count = size / (Int.bitWidth / 8)
    print("count: \(count)")
    let linker_set = UnsafeBufferPointer(start: start.bindMemory(to: Int.self, capacity: count), count: count)
    for i in 0 ..< linker_set.count {
      print("mysection[\(i)]: \(linker_set[i])")
    }
  }
}

// CHECK-SIL:      // i_am_not_referenced
// CHECK-SIL-NEXT: sil_global [serialized] [let] @$e8MyModule19i_am_not_referencedSivp : $Int = {

// CHECK: count: 1
// CHECK: mysection[0]: 42
