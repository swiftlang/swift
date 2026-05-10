// RUN: %target-swift-frontend -prespecialize-generic-metadata -target %target-cpu-apple-macosx12.0 -enable-emit-generic-class-ro_t-list -S %s -o - | %FileCheck %s
// REQUIRES: objc_interop
// REQUIRES: CPU=x86_64 || CPU=arm64
// REQUIRES: OS=macosx

// CHECK: ___unnamed_1:
// CHECK:         .quad   _OBJC_METACLASS_$__TtCs12_SwiftObject
// CHECK:         .quad   0
// CHECK:         .quad   __objc_empty_cache
// CHECK:         .quad   0
// CHECK:         .quad   0
// Start of rodata_t
// CHECK:         .long   128
// CHECK:         .long   16
// CHECK:         .long   16
// CHECK:         .long   0
// CHECK:         .quad   0
// CHECK:         .quad   0
// CHECK:         .quad   __INSTANCE_METHODS__TtC25generic_class_rodata_list9Something
// CHECK:         .quad   0
// CHECK:         .quad   0
// CHECK:         .quad   0
// CHECK:         .quad   0
// Start of rodata_t
// CHECK:         .long   129
// CHECK:         .long   40
// CHECK:         .long   40
// CHECK:         .long   0
// CHECK:         .quad   0
// CHECK:         .quad   0
// CHECK:         .quad   __CLASS_METHODS__TtC25generic_class_rodata_list9Somethin


// CHECK: __METACLASS_DATA_$s25generic_class_rodata_list9SomethingCySuGMf:
// CHECK:         .long   129
// CHECK:         .long   40
// CHECK:         .long   40
// CHECK:         .long   0
// CHECK:         .quad   _$s25generic_class_rodata_list9SomethingCySuGMf+24
// CHECK:         .quad   0
// CHECK:         .quad   __CLASS_METHODS_$s25generic_class_rodata_list9SomethingCySuGMf

// CHECK: __DATA_$s25generic_class_rodata_list9SomethingCySuGMf:
// CHECK:         .long   128
// CHECK:         .long   16
// CHECK:         .long   16
// CHECK:         .long   0
// CHECK:         .quad   0
// CHECK:         .quad   0
// CHECK:         .quad   __INSTANCE_METHODS_$s25generic_class_rodata_list9SomethingCySuGMf


// CHECK:        .section        __DATA,__objc_clsrolist
// CHECK:        .p2align        3
// CHECK:_generic_ro_datas:
// CHECK:        .quad   ___unnamed_1+40
// CHECK:        .quad   ___unnamed_1+112
// CHECK:        .quad   __METACLASS_DATA_$s25generic_class_rodata_list9SomethingCySuGMf
// CHECK:        .quad   __DATA_$s25generic_class_rodata_list9SomethingCySuGMf

import Foundation

public class Something<T> {
  @objc
  public func myMethod() { print("hello") }
  @objc
  public static func myStaticMethod() { print("static") }
}

public protocol P {
    func t<T>(_ t: T)
}

public func some(_ arr : Something<UInt>, p: P) {
  p.t(arr)
}
