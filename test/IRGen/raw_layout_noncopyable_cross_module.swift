// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -O -disable-availability-checking \
// RUN:   %t/Core.swift                                                      \
// RUN:   -module-name Core                                                  \
// RUN:   -emit-module-path %t/Core.swiftmodule                              \
// RUN:   -enable-experimental-feature RawLayout                             \
// RUN:   -enable-experimental-feature ValueGenerics

// RUN: %target-swift-frontend -emit-ir -O -disable-availability-checking    \
// RUN:   %t/Consumer.swift                                                  \
// RUN:   -I %t                                                              \
// RUN:   -module-name Consumer                                              \
// RUN:   -enable-experimental-feature RawLayout                             \
// RUN:   -enable-experimental-feature ValueGenerics                         \
// RUN:   | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_RawLayout
// REQUIRES: swift_feature_ValueGenerics

// Public ~Copyable types with @_rawLayout(likeArrayOf:count:) fields crash
// the LLVM verifier under -O with "Instruction does not dominate all uses!"
//
// Element-wise destruction generates incorrect invariant.load IR for
// @_rawLayout fields whose layout depends on generic parameters. The fix
// in GenStruct.cpp forces VWT-based destruction for ~Copyable types that
// contain @_rawLayout fields.

//--- Core.swift

public enum Storage<Element: ~Copyable> {
  @_rawLayout(likeArrayOf: Element, count: capacity)
  public struct Inline<let capacity: Int>: ~Copyable {
    public init() {}
    deinit {}
  }
}

//--- Consumer.swift

import Core

// Verify that Buffer's destroy uses VWT-based destruction (indirect call
// through the value witness table) instead of element-wise destruction.
//
// CHECK-LABEL: define {{.*}} void @"$s8Consumer6BufferVwxx"
// CHECK:         load ptr, ptr {{%.*}}, align 8, !invariant.load
// CHECK:         call void %Destroy(ptr {{.*}}
// CHECK:         ret void
// CHECK:       }
public struct Buffer<Element: ~Copyable>: ~Copyable {
  var _a: Storage<Element>.Inline<8>
  var _b: Storage<Element>.Inline<4>

  public init() {
    _a = .init()
    _b = .init()
  }
}
