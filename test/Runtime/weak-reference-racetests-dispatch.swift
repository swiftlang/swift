// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

import Dispatch

let iterations = 200_000

class Thing {}

class WBox<T: AnyObject> {
  weak var wref: T?
  init(_ ref: T) { self.wref = ref }
  init() { self.wref = nil }
}

let WeakReferenceRaceTests = TestSuite("WeakReferenceRaceTests")

WeakReferenceRaceTests.test("class instance property [SR-192] (copy)") {
  let q = dispatch_queue_create("", DISPATCH_QUEUE_CONCURRENT)!

  // Capture a weak reference via its container object
  // "https://bugs.swift.org/browse/SR-192"
  for i in 1...iterations {
    let box = WBox(Thing())
    let closure = {
      let nbox = WBox<Thing>()
      nbox.wref = box.wref
      _blackHole(nbox)
    }

    dispatch_async(q, closure)
    dispatch_async(q, closure)
  }

  dispatch_barrier_sync(q) {}
}

WeakReferenceRaceTests.test("class instance property [SR-192] (load)") {
  let q = dispatch_queue_create("", DISPATCH_QUEUE_CONCURRENT)!

  // Capture a weak reference via its container object
  // "https://bugs.swift.org/browse/SR-192"
  for i in 1...iterations {
    let box = WBox(Thing())
    let closure = {
      if let ref = box.wref {
        _blackHole(ref)
      }
    }

    dispatch_async(q, closure)
    dispatch_async(q, closure)
  }
  
  dispatch_barrier_sync(q) {}
}

WeakReferenceRaceTests.test("direct capture (copy)") {
  let q = dispatch_queue_create("", DISPATCH_QUEUE_CONCURRENT)!

  // Capture a weak reference directly in multiple closures
  for i in 1...iterations {
    weak var wref = Thing()
    let closure = {
      let nbox = WBox<Thing>()
      nbox.wref = wref
      _blackHole(nbox)
    }

    dispatch_async(q, closure)
    dispatch_async(q, closure)
  }

  dispatch_barrier_sync(q) {}
}

WeakReferenceRaceTests.test("direct capture (load)") {
  let q = dispatch_queue_create("", DISPATCH_QUEUE_CONCURRENT)!

  // Capture a weak reference directly in multiple closures
  for i in 1...iterations {
    weak var wref = Thing()
    let closure = {
      if let ref = wref {
        _blackHole(ref)
      }
    }

    dispatch_async(q, closure)
    dispatch_async(q, closure)
  }

  dispatch_barrier_sync(q) {}
}

runAllTests()
