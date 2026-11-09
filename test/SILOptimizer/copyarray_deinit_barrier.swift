// RUN: %target-swift-frontend -O -module-name main -emit-sil %s | %FileCheck %s

// Builtin.copyArray reads through a pointer into storage owned by a class
// object. It must be a deinit barrier: the object's release (which frees the
// storage in its deinit) must not be hoisted above the copy.

public final class Element {}

final class Storage {
  let base: UnsafeMutablePointer<Element>
  let count: Int
  init(count: Int) {
    self.count = count
    base = UnsafeMutablePointer<Element>.allocate(capacity: count)
    base.initialize(repeating: Element(), count: count)
  }
  deinit {
    base.deinitialize(count: count)
    base.deallocate()
  }
}

@inline(never)
func makeStorage(_ n: Int) -> Storage { Storage(count: n) }

// CHECK-LABEL: sil [noinline] @$s4main7copyOutySpyAA7ElementCGSiF :
// CHECK:         [[S:%[0-9]+]] = apply
// CHECK-NOT:     strong_release [[S]]
// CHECK:         builtin "copyArray"<Element>
// CHECK:         strong_release [[S]]
// CHECK:       } // end sil function '$s4main7copyOutySpyAA7ElementCGSiF'
@inline(never)
public func copyOut(_ n: Int) -> UnsafeMutablePointer<Element> {
  let storage = makeStorage(n)
  let dst = UnsafeMutablePointer<Element>.allocate(capacity: n)
  dst.initialize(from: storage.base, count: n)
  return dst
}

