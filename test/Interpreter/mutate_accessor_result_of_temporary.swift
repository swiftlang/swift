// RUN: %target-run-simple-swift(-Onone) | %FileCheck %s

// REQUIRES: executable_test

// The address result of a mutate accessor is only valid while its object self
// is alive. MandatoryDestroyHoisting must not destroy a temporary self before
// a write through the result.

final class OwningPointerBox {
  let storage: UnsafeMutablePointer<Int>

  init(_ v: Int) {
    storage = .allocate(capacity: 1)
    storage.initialize(to: v)
  }

  deinit {
    print("deinit: value =", storage.pointee)
    storage.deallocate()
  }

  var value: Int {
    @_unsafeSelfDependentResult
    borrow {
      return storage.pointee
    }
    @_unsafeSelfDependentResult
    mutate {
      return &storage.pointee
    }
  }
}

@inline(never)
func makeOwningPointerBox() -> OwningPointerBox {
  return OwningPointerBox(1)
}

@inline(never)
func test() {
  // The temporary is the only reference to the box.
  makeOwningPointerBox().value = 42
}

// CHECK: deinit: value = 42
test()
