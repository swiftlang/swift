// RUN: %empty-directory(%t/plain)
// RUN: %empty-directory(%t/cmo)
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -emit-ir -module-name PackageLibrary -package-name ClassAllocatorPackage -enable-library-evolution -O %S/../Interpreter/Inputs/class_allocator_package_library.swift | %FileCheck %s --check-prefix LIBRARY

// Without Package CMO, the client accesses the library's classes
// resiliently: it calls their initializers, and leaves the free at the last
// unowned release to the runtime.
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -emit-module -emit-module-path %t/plain/PackageLibrary.swiftmodule -module-name PackageLibrary -package-name ClassAllocatorPackage -enable-library-evolution -O -swift-version 5 %S/../Interpreter/Inputs/class_allocator_package_library.swift -emit-module-interface-path %t/plain/interface/PackageLibrary.swiftinterface -emit-package-module-interface-path %t/plain/interface/PackageLibrary.package.swiftinterface
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %s -package-name ClassAllocatorPackage -emit-ir -module-name Client -I %t/plain -O -disable-llvm-merge-functions-pass | %FileCheck %s --check-prefixes CHECK,RESILIENT
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %s -package-name ClassAllocatorPackage -emit-ir -module-name Client -I %t/plain -Onone | %FileCheck %s --check-prefixes CHECK,RESILIENT

// So does a client that imports the library from its package interface.
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %s -package-name ClassAllocatorPackage -emit-ir -module-name Client -I %t/plain/interface -experimental-package-interface-load -module-cache-path %t/module-cache -O -disable-llvm-merge-functions-pass | %FileCheck %s --check-prefixes CHECK,RESILIENT

// With Package CMO, it accesses them without resilience, inlines their
// allocations, and calls the allocator directly.
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -emit-module -emit-module-path %t/cmo/PackageLibrary.swiftmodule -module-name PackageLibrary -package-name ClassAllocatorPackage -enable-library-evolution -O -swift-version 5 %S/../Interpreter/Inputs/class_allocator_package_library.swift -allow-non-resilient-access -package-cmo
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %s -package-name ClassAllocatorPackage -emit-ir -module-name Client -I %t/cmo -O -disable-llvm-merge-functions-pass | %FileCheck %s --check-prefixes CHECK,CMO,CMO-O
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %s -package-name ClassAllocatorPackage -emit-ir -module-name Client -I %t/cmo -Onone | %FileCheck %s --check-prefixes CHECK,CMO,CMO-ONONE

// REQUIRES: swift_feature_ClassAllocators
// REQUIRES: CPU=arm64 || CPU=x86_64

import PackageLibrary

// The library's package classes point to the library's table for the pool.
// LIBRARY: @[[LIBRARY_TABLE:__swift_class_allocator_value_witnesses_14PackageLibrary0A4PoolO]] = linkonce_odr hidden constant {{.*}} ptr @"$s14PackageLibrary0A4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ" }
// LIBRARY: @"$s14PackageLibrary0A5FinalCMf" = internal global {{.*}} ptr @[[LIBRARY_TABLE]], {{.*}}, i32 34,
// The `open` class nested in a package type can't have an allocator.
// LIBRARY: @"$s14PackageLibrary9NamespaceO4BaseCMf" = internal global {{.*}} ptr @"$sBoWV", {{.*}}, i32 2,

// The client's subclass of the library's `open` class nested in a package
// type declares the library's pool. The relocation function of its metadata
// pattern (the superclass's metadata is resilient) installs the client's copy
// of the pool's table, whose deallocate entry is the library's method.
// CHECK: @[[TABLE:__swift_class_allocator_value_witnesses_14PackageLibrary0A4PoolO]] = linkonce_odr hidden constant {{.*}} ptr @"$s14PackageLibrary0A4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ" }
// CHECK: @"$s6Client0A3SubCMP" = internal constant {{.*}} @"__swift_relocate_class_metadata_$s6Client0A3SubCMn"
// CHECK: @[[CLIENT_TABLE:__swift_class_allocator_value_witnesses_6Client0A4PoolO]] = linkonce_odr hidden constant
// CHECK: @"$s6Client0A3OwnCMP" = internal constant {{.*}} @"__swift_relocate_class_metadata_$s6Client0A3OwnCMn"
@allocator(PackagePool)
final class ClientSub: Namespace.Base {
  var y = 2
  var vector = SIMD4<Double>(repeating: 1)
}

enum ClientPool: @unsafe ClassAllocator {
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
  }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe pointer.deallocate()
  }
}

@allocator(ClientPool)
final class ClientOwn: Namespace.Base {}

final class UnownedFinalHolder {
  unowned let target: PackageFinal
  init(_ target: PackageFinal) { self.target = target }
}

final class UnownedSubHolder {
  unowned let target: ClientSub
  init(_ target: ClientSub) { self.target = target }
}

// The client allocates its subclass's instances with the pool.
// With Package CMO, their size is a constant; otherwise it depends on the
// library's class, and the client calls the pool through its conformance.
// CHECK-LABEL: define {{.*}}@"$s6Client0A3SubCACycfC"(
// CHECK-NOT: @swift_allocObject(
// CMO-O: call swiftcc ptr @"$s14PackageLibrary0A4PoolO8allocate9byteCount9alignment3forSvSi_SiyXlXptFZ"(i64 64, i64 16, ptr
// CMO-O: call swiftcc ptr @swift_initHeapObject(ptr {{.*}}, i64 64, i64 15)
// CMO-ONONE: call swiftcc ptr @"$ss22_allocateClassInstance{{.*}}"(ptr %0, ptr @"$s14PackageLibrary0A4PoolON", i64 64, i64 15,
// RESILIENT: load i32
// RESILIENT: call swiftcc ptr @"$ss{{(14ClassAllocatorP8allocate|22_allocateClassInstance)}}
// CHECK: }

// ...and frees them with it.
// CHECK-LABEL: define {{.*}}@"$s6Client0A3SubCfD"(
// CHECK-NOT: @swift_deallocClassInstance(
// CMO-O: [[ALLOCATION:%.*]] = {{(tail )?}}call swiftcc ptr @swift_deallocClassInstanceBegin(
// CMO-O: call swiftcc void @"$s14PackageLibrary0A4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr {{(nonnull )?}}[[ALLOCATION]], i64 64, i64 16,
// CMO-ONONE: call swiftcc void @"$ss24_deallocateClassInstance{{.*}}"(ptr %{{.*}}, ptr @"$s14PackageLibrary0A4PoolON", ptr %{{.*}}, i64 64, i64 15,
// RESILIENT: load i32
// RESILIENT: call swiftcc void @"$ss{{(14ClassAllocatorP10deallocate|24_deallocateClassInstance)}}
// CHECK: }

// The subclass with its own allocator uses it.
// CHECK-LABEL: define {{.*}}@"$s6Client0A3OwnCfD"(
// CHECK-NOT: PackageLibrary0A4PoolO
// CHECK: }

// The last unowned release of an instance of the library's final package
// class: with Package CMO, the client frees it directly, with constants;
// otherwise the library could change the class's allocator, so the runtime
// frees it.
// CHECK-LABEL: define {{.*}}@"$s6Client18UnownedFinalHolderCfd"(
// CMO: [[ALLOCATION:%.*]] = {{(tail )?}}call ptr @swift_unownedReleaseBegin(
// CMO: call swiftcc void @"$s14PackageLibrary0A4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr {{(nonnull )?}}[[ALLOCATION]], i64 24, i64 8,
// RESILIENT-NOT: @swift_unownedReleaseBegin(
// RESILIENT-NOT: PoolO10deallocate
// RESILIENT: call void @swift_unownedRelease(
// RESILIENT-NOT: PoolO10deallocate
// CHECK: }

// The client's own final class: the client frees it directly, with
// constants if it knows the superclass's layout, and otherwise with the size
// and alignment in the instance's metadata.
// CHECK-LABEL: define {{.*}}@"$s6Client16UnownedSubHolderCfd"(
// CHECK: [[ALLOCATION:%.*]] = {{(tail )?}}call ptr @swift_unownedReleaseBegin(
// CMO: call swiftcc void @"$s14PackageLibrary0A4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr {{(nonnull )?}}[[ALLOCATION]], i64 64, i64 16,
// RESILIENT: [[SIZE32:%.*]] = load i32
// RESILIENT: [[SIZE:%.*]] = zext i32 [[SIZE32]] to i64
// RESILIENT: call swiftcc void @"$s14PackageLibrary0A4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr {{(nonnull )?}}[[ALLOCATION]], i64 [[SIZE]],
// CHECK: }

// With Package CMO and optimization, the library's initializer is inlined, and
// the client calls the pool directly with the instance's size and alignment.
// Otherwise the client calls the library's initializer.
// CHECK-LABEL: define {{.*}}@"$s6Client9makeFinal14PackageLibrary0dC0CyF"(
// CHECK-NOT: @swift_allocObject(
// CMO-O: call swiftcc ptr @"$s14PackageLibrary0A4PoolO8allocate9byteCount9alignment3forSvSi_SiyXlXptFZ"(i64 24, i64 8, ptr
// CMO-O: call swiftcc ptr @swift_initHeapObject(ptr {{.*}}, i64 24, i64 7)
// CMO-ONONE: call swiftcc ptr @"$s14PackageLibrary0A5FinalCyACSicfC"(
// RESILIENT-NOT: PoolO8allocate
// RESILIENT: call swiftcc ptr @"$s14PackageLibrary0A5FinalCyACSicfC"(
// CHECK: }
package func makeFinal() -> PackageFinal {
  return PackageFinal(1)
}

// CHECK-LABEL: define {{.*}}@"__swift_relocate_class_metadata_$s6Client0A3SubCMn"(
// CHECK: store ptr @[[TABLE]],
// CHECK: }

// CHECK-LABEL: define {{.*}}@"__swift_relocate_class_metadata_$s6Client0A3OwnCMn"(
// CHECK: store ptr @[[CLIENT_TABLE]],
// CHECK: }
