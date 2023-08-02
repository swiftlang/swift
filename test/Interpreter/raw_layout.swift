// RUN: %target-run-simple-swift(-enable-experimental-feature RawLayout -enable-builtin-module) | %FileCheck %s
// REQUIRES: executable_test

import Builtin

@_rawLayout(like: T)
struct Cell<T>: ~Copyable {
  var address: UnsafeMutablePointer<T> {
    UnsafeMutablePointer<T>(Builtin.unprotectedAddressOfBorrow(self))
  }

  init(_ value: consuming T) {
    address.initialize(to: value)
  }

  deinit {
    // FIXME: discard self should work with rawLayout types
    //address.deinitialize(count: 1)

    // Note: We don't need to deallocate the address here because the memory it
    // points to is being destroyed within this deinit.
  }

  borrowing func replace(with replacement: consuming T) -> T {
    let previous = address.move()
    address.initialize(to: replacement)
    return previous
  }

  borrowing func set(_ value: consuming T) {
    let previous = replace(with: value)
    _ = consume previous
  }

  consuming func get() -> T {
    // Move the value out of self and don't call our deinitializer
    let previous = address.move()
    discard self
    return previous
  }
}

@_eagerMove
class SpecialInt {
  var value: Int

  init(_ value: Int) {
    self.value = value
  }

  deinit {
    print("Deinitializing \(value)!")
  }
}

do {
  let specialInt0 = SpecialInt(128)
  let cell0 = Cell<SpecialInt>(specialInt0)

  let specialInt0Again = cell0.replace(with: SpecialInt(316))

  // CHECK: Deinitializing 316!
  cell0.set(SpecialInt(592))

  let specialInt1 = cell0.get()

  // CHECK: Deinitializing 592!
  // CHECK: Deinitializing 128!
}
