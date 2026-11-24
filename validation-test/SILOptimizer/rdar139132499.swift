// RUN: %target-swift-frontend -emit-sil -O %s | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

public protocol Thing {}
public struct Thing0: Thing {}
public struct Thing1: Thing {}
public struct Thing2: Thing {}
public struct Thing3: Thing {}

public struct ThingBundle {
  public var things: [any Thing]

  public init() {
    self.things = []
  }

  @inline(always)
  public init<each T: Thing>(things: repeat each T) {
    self.init()
    _ = { (b: inout [any Thing]) in
      repeat b.append(each things)
    }(&self.things)
  }

  @inline(always)
  public init(_ t0: Thing0, _ t1: Thing1, _ t2: Thing2, _ t3: Thing3) {
    self.init()
    _ = { (b: inout [any Thing]) in
      b.append(t0)
      b.append(t1)
      b.append(t2)
      b.append(t3)
    }(&self.things)
  }
}

// rdar://139132499: Optimization opportunities for inlined variadic function
//
// The dynamic pack loops in the function are all fully unrolled and
// specialized, generating an equivalent CFG to the no-packs variant. This is
// despite the variadic S.init being marked @inline(always), which used to block
// certain pack optimizations.
//
// CHECK-LABEL: sil @$s13rdar13913249910bundlePackAA11ThingBundleVyF : $@convention(thin) () -> @owned ThingBundle {
// CHECK-NOT:     alloc_pack
// CHECK-NOT:     pack_element
// CHECK:         {{^bb12}}
// CHECK-NOT:     {{^bb}}
// CHECK-NOT:     alloc_pack
// CHECK-NOT:     pack_element
// CHECK-LABEL: } // end sil function '$s13rdar13913249910bundlePackAA11ThingBundleVyF'
public func bundlePack() -> ThingBundle {
  ThingBundle(things: Thing0(), Thing1(), Thing2(), Thing3())
}

// CHECK-LABEL: sil @$s13rdar13913249912bundleNoPackAA11ThingBundleVyF : $@convention(thin) () -> @owned ThingBundle {
// CHECK:         {{^bb12}}
// CHECK-NOT:     {{^bb}}
// CHECK-LABEL: } // end sil function '$s13rdar13913249912bundleNoPackAA11ThingBundleVyF'
public func bundleNoPack() -> ThingBundle {
  ThingBundle(Thing0(), Thing1(), Thing2(), Thing3())
}
