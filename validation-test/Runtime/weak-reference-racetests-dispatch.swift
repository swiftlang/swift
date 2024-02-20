// RUN: %target-build-swift %s %import-libdispatch -o %t_binary
// RUN: %target-codesign %t_binary
// RUN: %target-run %t_binary
// REQUIRES: executable_test
// REQUIRES: stress_test
// REQUIRES: libdispatch
// UNSUPPORTED: threading_none

import StdlibUnittest
import Dispatch

let iterations = 200_000

class Thing {}

class WBox<T: AnyObject> {
  weak var wref: T?
  init(_ ref: T) { self.wref = ref }
  init() { self.wref = nil }
}

let WeakReferenceRaceTests = TestSuite("WeakReferenceRaceTests")

func forwardOptional<T>(_ t: T!) -> T {
  return t!
}

WeakReferenceRaceTests.test("class instance property (copy)") {
  let q = DispatchQueue(label: "", attributes: .concurrent)

  // Capture a weak reference via its container object.
  // https://github.com/apple/swift/issues/42814
  for i in 1...iterations {
    let box = WBox(Thing())
    let closure = {
      let nbox = WBox<Thing>()
      nbox.wref = box.wref
      _blackHole(nbox)
    }

    q.async(execute: closure)
    q.async(execute: closure)
  }

  q.async(flags: .barrier) {}
}

WeakReferenceRaceTests.test("class instance property (load)") {
  let q = DispatchQueue(label: "", attributes: .concurrent)

  // Capture a weak reference via its container object.
  // https://github.com/apple/swift/issues/42814
  for i in 1...iterations {
    let box = WBox(Thing())
    let closure = {
      if let ref = box.wref {
        _blackHole(ref)
      }
    }

    q.async(execute: closure)
    q.async(execute: closure)
  }

  q.async(flags: .barrier) {}
}

WeakReferenceRaceTests.test("direct capture (copy)") {
  let q = DispatchQueue(label: "", attributes: .concurrent)

  // Capture a weak reference directly in multiple closures
  for i in 1...iterations {
    weak var wref = Thing()
    let closure = {
      let nbox = WBox<Thing>()
      nbox.wref = wref
      _blackHole(nbox)
    }

    q.async(execute: closure)
    q.async(execute: closure)
  }

  q.async(flags: .barrier) {}
}

WeakReferenceRaceTests.test("direct capture (load)") {
  let q = DispatchQueue(label: "", attributes: .concurrent)

  // Capture a weak reference directly in multiple closures
  for i in 1...iterations {
    weak var wref = Thing()
    let closure = {
      if let ref = wref {
        _blackHole(ref)
      }
    }

    q.async(execute: closure)
    q.async(execute: closure)
  }

  q.async(flags: .barrier) {}
}

runAllTests()
