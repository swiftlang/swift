// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -primary-file %s -emit-ir -module-name M -O > %t.O.ll
// RUN: %FileCheck %s < %t.O.ll
// RUN: %FileCheck %s --check-prefix=DECL < %t.O.ll
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -primary-file %s -emit-ir -module-name M -Onone > %t.Onone.ll
// RUN: %FileCheck %s < %t.Onone.ll
// RUN: %FileCheck %s --check-prefix=DECL < %t.Onone.ll

// REQUIRES: swift_feature_ClassAllocators
// REQUIRES: OS=macosx

// The runtime entry points for class allocators are only called for classes
// with allocators, which need a new runtime. When deploying to older
// runtimes, code without such classes doesn't reference them, and classes
// that are available only on newer runtimes reference them weakly.

public class NonFinal {
  public required init() {}
}
public final class Final {}

public final class Holder {
  public unowned let nonFinal: NonFinal
  public unowned let final: Final
  public init(_ nonFinal: NonFinal, _ final: Final) {
    self.nonFinal = nonFinal
    self.final = final
  }
}

public func make(_ type: NonFinal.Type) -> NonFinal {
  return type.init()
}

@available(macOS 99.99, *)
public enum Pool: @unsafe ClassAllocator {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
  }
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe pointer.deallocate()
  }
}

@available(macOS 99.99, *)
@allocator(Pool)
public final class New {
  public init() {}
}

@available(macOS 99.99, *)
public final class NewHolder {
  public unowned let new: New
  public init(_ new: New) { self.new = new }
}

// DECL-DAG: declare extern_weak {{.*}}@swift_initHeapObject(
// DECL-DAG: declare extern_weak {{.*}}@swift_deallocClassInstanceBegin(
// DECL-DAG: declare extern_weak {{.*}}@swift_unownedReleaseBegin(

// CHECK-LABEL: define {{.*}}@"$s1M6HolderCfd"(
// CHECK-NOT: swift_unownedReleaseBegin
// CHECK: }

// CHECK-LABEL: define {{.*}}@"$s1M4makeyAA8NonFinalCADmF"(
// CHECK-NOT: swift_initHeapObject
// CHECK-NOT: class_allocator
// CHECK: }
