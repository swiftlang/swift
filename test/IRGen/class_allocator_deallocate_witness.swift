// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// The `deallocate` class allocator witness of a non-generic enum or struct
// allocator whose witness is its own method is that method, in the module
// that declares the allocator and in its clients, whether the method is
// public, @inlinable or @_alwaysEmitIntoClient (which has no symbol in its
// module: each module that uses it emits its own copy, as it does the table).
// Other allocators get a function with the same signature that calls their
// witness.

// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple %t/Pools.swift -emit-module -emit-module-path %t/Pools.swiftmodule -module-name Pools
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple %t/Pools.swift -emit-ir -module-name Pools -Onone | %FileCheck %t/Pools.swift --check-prefix=LIB
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple %t/Pools.swift -emit-ir -module-name Pools -O | %FileCheck %t/Pools.swift --check-prefix=LIB
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %t/Client.swift -I %t -emit-ir -module-name Client -Onone | %FileCheck %t/Client.swift --check-prefixes=CHECK,ONONE
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %t/Client.swift -I %t -emit-ir -module-name Client -O | %FileCheck %t/Client.swift --check-prefixes=CHECK,OPT

// The same with a library built with library evolution.
// RUN: %empty-directory(%t/resilient)
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple %t/Pools.swift -emit-module -emit-module-path %t/resilient/Pools.swiftmodule -module-name Pools -enable-library-evolution
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple %t/Pools.swift -emit-ir -module-name Pools -Onone -enable-library-evolution | %FileCheck %t/Pools.swift --check-prefix=LIB
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %t/Client.swift -I %t/resilient -emit-ir -module-name Client -Onone | %FileCheck %t/Client.swift --check-prefixes=CHECK,ONONE
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %t/Client.swift -I %t/resilient -emit-ir -module-name Client -O | %FileCheck %t/Client.swift --check-prefixes=CHECK,OPT

// REQUIRES: swift_feature_ClassAllocators
// REQUIRES: CPU=arm64 || CPU=x86_64

//--- Pools.swift

@inline(never)
public func poolAllocate(_ byteCount: Int, _ alignment: Int) -> UnsafeMutableRawPointer {
  UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
}

@inline(never)
public func poolDeallocate(_ pointer: UnsafeMutableRawPointer) {
  unsafe pointer.deallocate()
}

public enum PlainPool: @unsafe ClassAllocator {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    unsafe poolAllocate(byteCount, alignment)
  }
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe poolDeallocate(pointer)
  }
}

public struct InlinablePool: @unsafe ClassAllocator {
  @inlinable
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    unsafe poolAllocate(byteCount, alignment)
  }
  @inlinable
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe poolDeallocate(pointer)
  }
}

public enum EmittedPool: @unsafe ClassAllocator {
  @_alwaysEmitIntoClient
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    unsafe poolAllocate(byteCount, alignment)
  }
  @_alwaysEmitIntoClient
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe poolDeallocate(pointer)
  }
}

public final class ClassPool: @unsafe ClassAllocator {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    unsafe poolAllocate(byteCount, alignment)
  }
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe poolDeallocate(pointer)
  }
}

@allocator(EmittedPool)
final class LibraryEmitted {}

// The module emits its own copy of the @_alwaysEmitIntoClient method for its
// table.
// LIB: @__swift_class_allocator_value_witnesses_5Pools11EmittedPoolO = linkonce_odr hidden constant {{.*}}, ptr @"$s5Pools11EmittedPoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ" }
// LIB: @llvm.used = {{.*}}ptr @"$s5Pools11EmittedPoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"
// LIB: define linkonce_odr hidden swiftcc void @"$s5Pools11EmittedPoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr {{.*}}%0, i64 %1, i64 %2, ptr {{.*}}%3)
// LIB-NOT: @__swift_class_allocator_deallocate_

//--- Client.swift

import Pools

@allocator(PlainPool)
public final class Plain {}

@allocator(InlinablePool)
public final class Inlinable {}

@allocator(EmittedPool)
public final class Emitted {}

@allocator(ClassPool)
public final class ClassAllocated {}

// CHECK-DAG: @__swift_class_allocator_value_witnesses_5Pools9PlainPoolO = linkonce_odr hidden constant {{.*}}, ptr @"$s5Pools9PlainPoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ" }
// CHECK-DAG: @__swift_class_allocator_value_witnesses_5Pools13InlinablePoolV = linkonce_odr hidden constant {{.*}}, ptr @"$s5Pools13InlinablePoolV10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ" }
// CHECK-DAG: @__swift_class_allocator_value_witnesses_5Pools11EmittedPoolO = linkonce_odr hidden constant {{.*}}, ptr @"$s5Pools11EmittedPoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ" }
// CHECK-DAG: @__swift_class_allocator_value_witnesses_5Pools9ClassPoolC = linkonce_odr hidden constant {{.*}}, ptr @__swift_class_allocator_deallocate_5Pools9ClassPoolC }

// The client emits its own copy of the @_alwaysEmitIntoClient method, and
// references the library's symbols of the others.
// CHECK-DAG: define linkonce_odr hidden swiftcc void @"$s5Pools11EmittedPoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr {{.*}}%0, i64 %1, i64 %2, ptr {{.*}}%3)
// CHECK-DAG: declare swiftcc void @"$s5Pools9PlainPoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr, i64, i64, ptr)
// CHECK-DAG: declare swiftcc void @"$s5Pools13InlinablePoolV10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr, i64, i64, ptr)

// The class allocator's function calls its method directly.
// CHECK-DAG: define linkonce_odr hidden swiftcc void @__swift_class_allocator_deallocate_5Pools9ClassPoolC(ptr %0, i64 %1, i64 %2, ptr %3)
// ONONE-DAG: call swiftcc void @"$s5Pools9ClassPoolC10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr %0, i64 %1, i64 %2, ptr %3, ptr swiftself {{%.*}})
// OPT-DAG: call swiftcc void @"$s5Pools9ClassPoolC10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr %0, i64 %1, i64 %2, ptr %3, ptr swiftself {{%.*}})

// CHECK-NOT: @__swift_class_allocator_deallocate_5Pools9PlainPoolO
// CHECK-NOT: @__swift_class_allocator_deallocate_5Pools13InlinablePoolV
// CHECK-NOT: @__swift_class_allocator_deallocate_5Pools11EmittedPoolO
