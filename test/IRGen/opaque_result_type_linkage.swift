// RUN: %target-swift-frontend -Xllvm -type-lowering-disable-verification -emit-ir -primary-file %s %S/Inputs/opaque_result_type_linkage_2.swift -module-name lib | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -type-lowering-disable-verification -emit-ir %s -primary-file %S/Inputs/opaque_result_type_linkage_2.swift -module-name lib | %FileCheck %s --check-prefix=OTHER

protocol P {
  associatedtype A
  var a: A { get }
}

struct G<T>: P {
  lazy var lazyVar: A = a

  var a: some Any {
    b()
  }

  consuming func b() -> some Any {
    "hello"
  }
}

// We lower the two outlined destroy functions differently depending on the type
// expansion context (there is an opaque result type).
// We therefore must not use hidden linkonce_odr linkage.

// CHECK: define{{.*}} private ptr @"$s3lib1GVyxGlWOh"(ptr %0)
// OTHER: define{{.*}} private ptr @"$s3lib1GVyxGlWOh"(ptr %0, ptr %"some Any", ptr %"Optional<some Any>", ptr %"G<T>")
