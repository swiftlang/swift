// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// In a whole-module compilation, the last unowned release of an instance of a
// non-final class calls the allocator directly if no class outside the module
// can subclass it and all of its subclasses have the same allocator.
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple %t/main.swift %t/other.swift -emit-ir -module-name M -package-name Package -Onone | %FileCheck %t/main.swift --check-prefixes=CHECK,WMO
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple %t/main.swift %t/other.swift -emit-ir -module-name M -package-name Package -O -disable-llvm-merge-functions-pass | %FileCheck %t/main.swift --check-prefix=OPT

// With -enable-testing or -enable-private-imports, importers can subclass the
// module's classes, and without whole-module optimization other files aren't
// seen: only final classes qualify.
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple %t/main.swift %t/other.swift -emit-ir -module-name M -package-name Package -Onone -enable-testing | %FileCheck %t/main.swift --check-prefixes=CHECK,RUNTIME
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple %t/main.swift %t/other.swift -emit-ir -module-name M -package-name Package -Onone -enable-private-imports | %FileCheck %t/main.swift --check-prefixes=CHECK,RUNTIME
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %t/main.swift %t/other.swift -emit-ir -module-name M -package-name Package -Onone | %FileCheck %t/main.swift --check-prefixes=CHECK,RUNTIME
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple -primary-file %t/main.swift %t/other.swift -emit-ir -module-name M -package-name Package -O | %FileCheck %t/main.swift --check-prefix=OPT-RUNTIME

// -default-class-allocator: the nearest declaration wins.
// RUN: %target-swift-frontend -enable-experimental-feature ClassAllocators -target %target-future-triple %t/default.swift -emit-ir -module-name D -Onone -default-class-allocator D.DefaultPool | %FileCheck %t/default.swift --check-prefix=DEFAULT

// REQUIRES: swift_feature_ClassAllocators
// REQUIRES: CPU=arm64 || CPU=x86_64

//--- main.swift

@inline(never)
func poolAllocate(_ byteCount: Int, _ alignment: Int) -> UnsafeMutableRawPointer {
  UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
}

@inline(never)
func poolDeallocate(_ pointer: UnsafeMutableRawPointer) {
  unsafe pointer.deallocate()
}

public enum Pool: @unsafe ClassAllocator {
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

public enum OtherPool: @unsafe ClassAllocator {
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

// Every subclass, including generic, nested, local and private ones in
// another file (other.swift), inherits Pool.
@allocator(Pool)
class Uniform {
  init() {}
  class Nested: Uniform {}
}
class UniformSub: Uniform {}
class UniformGenericSub<T>: UniformSub {}
@allocator(Pool)
class UniformRedeclared: Uniform {}
func makeLocalUniform() -> Uniform {
  class Local: Uniform {}
  return Local()
}

// A public class that isn't open can't be subclassed outside the module.
@allocator(Pool)
public class PublicNotOpen {
  public init() {}
}

// A generic class qualifies too.
@allocator(Pool)
class GenericBase<T> {
  init() {}
}
class GenericBaseSub: GenericBase<Int> {}

// A private subclass in other.swift opts out.
@allocator(Pool)
class OptedOutElsewhere {
  init() {}
}

// A local subclass in other.swift has a different allocator.
@allocator(Pool)
class DifferentElsewhere {
  init() {}
}

// A grandchild has a different allocator: neither the class nor its child
// qualifies.
@allocator(Pool)
class GrandchildDiffers {
  init() {}
}
class GrandchildDiffersChild: GrandchildDiffers {}
@allocator(OtherPool)
class GrandchildDiffersGrandchild: GrandchildDiffersChild {}

// Clients can subclass an open class.
@allocator(Pool)
open class Open {
  public init() {}
}

// The other modules of the package can subclass an open class nested in a
// package type, although its effective access is only `package`. (It may
// have an allocator only because this module isn't built with library
// evolution.)
package enum PackageNamespace {
  @allocator(Pool)
  open class OpenInPackage {
    package init() {}
  }
}

final class HoldUniform {
  unowned let r: Uniform
  init(_ r: Uniform) { self.r = r }
}
final class HoldUniformSub {
  unowned let r: UniformSub
  init(_ r: UniformSub) { self.r = r }
}
final class HoldPublicNotOpen {
  unowned let r: PublicNotOpen
  init(_ r: PublicNotOpen) { self.r = r }
}
final class HoldGenericBase {
  unowned let r: GenericBase<String>
  init(_ r: GenericBase<String>) { self.r = r }
}
final class HoldOptedOutElsewhere {
  unowned let r: OptedOutElsewhere
  init(_ r: OptedOutElsewhere) { self.r = r }
}
final class HoldDifferentElsewhere {
  unowned let r: DifferentElsewhere
  init(_ r: DifferentElsewhere) { self.r = r }
}
final class HoldGrandchildDiffers {
  unowned let r: GrandchildDiffers
  init(_ r: GrandchildDiffers) { self.r = r }
}
final class HoldGrandchildDiffersChild {
  unowned let r: GrandchildDiffersChild
  init(_ r: GrandchildDiffersChild) { self.r = r }
}
final class HoldOpenInPackage {
  unowned let r: PackageNamespace.OpenInPackage
  init(_ r: PackageNamespace.OpenInPackage) { self.r = r }
}
final class HoldOpen {
  unowned let r: Open
  init(_ r: Open) { self.r = r }
}

// The size, alignment and metadata passed to the allocator are the
// instance's own, loaded from the instance.
// CHECK-LABEL: define {{.*}}swiftcc {{.*}}@"$s1M11HoldUniformCfd"(
// WMO: [[ALLOCATION:%.*]] = {{(tail )?}}call ptr @swift_unownedReleaseBegin(ptr [[OBJECT:%.*]])
// WMO: br i1 {{%.*}}, label %unowned.free, label %unowned.released
// WMO: unowned.free:
// WMO: [[METADATA:%.*]] = load ptr, ptr [[OBJECT]]
// WMO: [[SIZE_ADDR:%.*]] = getelementptr inbounds i8, ptr [[METADATA]], i32 {{[0-9]+}}
// WMO: [[SIZE32:%.*]] = load i32, ptr [[SIZE_ADDR]]
// WMO: [[SIZE:%.*]] = zext i32 [[SIZE32]] to i64
// WMO: [[ALIGN_ADDR:%.*]] = getelementptr inbounds i8, ptr [[METADATA]], i32 {{[0-9]+}}
// WMO: [[ALIGN16:%.*]] = load i16, ptr [[ALIGN_ADDR]]
// WMO: [[ALIGNMASK:%.*]] = zext i16 [[ALIGN16]] to i64
// WMO: [[ALIGN:%.*]] = add i64 [[ALIGNMASK]], 1
// WMO: call swiftcc void @"$s1M4PoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(ptr [[ALLOCATION]], i64 [[SIZE]], i64 [[ALIGN]], ptr [[METADATA]])
// RUNTIME-NOT: @swift_unownedReleaseBegin(
// RUNTIME: call void @swift_unownedRelease(
// CHECK: }

// With optimization, the allocator is inlined.
// OPT-LABEL: define {{.*}}swiftcc void @"$s1M11HoldUniformCfD"(
// OPT: [[ALLOCATION:%.*]] = {{(tail )?}}call ptr @swift_unownedReleaseBegin(
// OPT: call swiftcc void @"$s1M14poolDeallocateyySvF"(ptr nonnull [[ALLOCATION]])
// OPT: }
// OPT-RUNTIME-LABEL: define {{.*}}swiftcc void @"$s1M11HoldUniformCfD"(
// OPT-RUNTIME-NOT: @swift_unownedReleaseBegin(
// OPT-RUNTIME: call void @swift_unownedRelease(
// OPT-RUNTIME-NOT: @swift_unownedReleaseBegin(
// OPT-RUNTIME: }

// CHECK-LABEL: define {{.*}}swiftcc {{.*}}@"$s1M14HoldUniformSubCfd"(
// WMO: call ptr @swift_unownedReleaseBegin(
// RUNTIME-NOT: @swift_unownedReleaseBegin(
// CHECK: }

// CHECK-LABEL: define {{.*}}swiftcc {{.*}}@"$s1M17HoldPublicNotOpenCfd"(
// WMO: call ptr @swift_unownedReleaseBegin(
// RUNTIME-NOT: @swift_unownedReleaseBegin(
// CHECK: }

// CHECK-LABEL: define {{.*}}swiftcc {{.*}}@"$s1M15HoldGenericBaseCfd"(
// WMO: call ptr @swift_unownedReleaseBegin(
// RUNTIME-NOT: @swift_unownedReleaseBegin(
// CHECK: }

// CHECK-LABEL: define {{.*}}swiftcc {{.*}}@"$s1M21HoldOptedOutElsewhereCfd"(
// CHECK-NOT: @swift_unownedReleaseBegin(
// CHECK: call void @swift_unownedRelease(
// CHECK-NOT: @swift_unownedReleaseBegin(
// CHECK: }

// CHECK-LABEL: define {{.*}}swiftcc {{.*}}@"$s1M22HoldDifferentElsewhereCfd"(
// CHECK-NOT: @swift_unownedReleaseBegin(
// CHECK: call void @swift_unownedRelease(
// CHECK-NOT: @swift_unownedReleaseBegin(
// CHECK: }

// CHECK-LABEL: define {{.*}}swiftcc {{.*}}@"$s1M21HoldGrandchildDiffersCfd"(
// CHECK-NOT: @swift_unownedReleaseBegin(
// CHECK: call void @swift_unownedRelease(
// CHECK-NOT: @swift_unownedReleaseBegin(
// CHECK: }

// CHECK-LABEL: define {{.*}}swiftcc {{.*}}@"$s1M26HoldGrandchildDiffersChildCfd"(
// CHECK-NOT: @swift_unownedReleaseBegin(
// CHECK: call void @swift_unownedRelease(
// CHECK-NOT: @swift_unownedReleaseBegin(
// CHECK: }

// CHECK-LABEL: define {{.*}}swiftcc {{.*}}@"$s1M17HoldOpenInPackageCfd"(
// CHECK-NOT: @swift_unownedReleaseBegin(
// CHECK: call void @swift_unownedRelease(
// CHECK-NOT: @swift_unownedReleaseBegin(
// CHECK: }

// CHECK-LABEL: define {{.*}}swiftcc {{.*}}@"$s1M8HoldOpenCfd"(
// CHECK-NOT: @swift_unownedReleaseBegin(
// CHECK: call void @swift_unownedRelease(
// CHECK-NOT: @swift_unownedReleaseBegin(
// CHECK: }


//--- other.swift

@allocator(SystemClassAllocator)
private class OptedOut: OptedOutElsewhere {}

func makeLocalDifferent() -> DifferentElsewhere {
  @allocator(OtherPool)
  class Local: DifferentElsewhere {}
  return Local()
}

func makeOptedOut() -> OptedOutElsewhere { OptedOut() }

// More subclasses of Uniform, which keep its allocator.
private class PrivateUniformSub: Uniform {}
extension Uniform {
  class NestedInExtension: Uniform {}
}

//--- default.swift

enum DefaultPool: @unsafe ClassAllocator {
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

enum OtherPool: @unsafe ClassAllocator {
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

// The default applies to the class and its subclass.
class DefaultUniform {}
class DefaultUniformSub: DefaultUniform {}

// A subclass opts out of the default.
class DefaultMixed {}
@allocator(SystemClassAllocator)
class DefaultMixedSub: DefaultMixed {}

// A subclass inherits the explicit allocator of its superclass rather than
// the default.
@allocator(OtherPool)
class Explicit {}
class ExplicitSub: Explicit {}

// An opted-out class has no allocator, nor does its subclass.
@allocator(SystemClassAllocator)
class OptedOutBase {}
class OptedOutBaseSub: OptedOutBase {}

final class HoldDefaultUniform {
  unowned let r: DefaultUniform
  init(_ r: DefaultUniform) { self.r = r }
}
final class HoldDefaultMixed {
  unowned let r: DefaultMixed
  init(_ r: DefaultMixed) { self.r = r }
}
final class HoldExplicit {
  unowned let r: Explicit
  init(_ r: Explicit) { self.r = r }
}
final class HoldOptedOutBase {
  unowned let r: OptedOutBase
  init(_ r: OptedOutBase) { self.r = r }
}

// DEFAULT-LABEL: define {{.*}}swiftcc {{.*}}@"$s1D18HoldDefaultUniformCfd"(
// DEFAULT: call ptr @swift_unownedReleaseBegin(
// DEFAULT: call swiftcc void @"$s1D11DefaultPoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(
// DEFAULT: }

// DEFAULT-LABEL: define {{.*}}swiftcc {{.*}}@"$s1D16HoldDefaultMixedCfd"(
// DEFAULT-NOT: @swift_unownedReleaseBegin(
// DEFAULT: call void @swift_unownedRelease(
// DEFAULT-NOT: @swift_unownedReleaseBegin(
// DEFAULT: }

// DEFAULT-LABEL: define {{.*}}swiftcc {{.*}}@"$s1D12HoldExplicitCfd"(
// DEFAULT: call ptr @swift_unownedReleaseBegin(
// DEFAULT: call swiftcc void @"$s1D9OtherPoolO10deallocate_9byteCount9alignment3forySv_S2iyXlXptFZ"(
// DEFAULT: }

// DEFAULT-LABEL: define {{.*}}swiftcc {{.*}}@"$s1D16HoldOptedOutBaseCfd"(
// DEFAULT-NOT: @swift_unownedReleaseBegin(
// DEFAULT: call void @swift_unownedRelease(
// DEFAULT-NOT: @swift_unownedReleaseBegin(
// DEFAULT: }
