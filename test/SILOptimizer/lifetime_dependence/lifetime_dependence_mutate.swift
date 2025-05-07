// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_LifetimeDependence

struct MutableSpan : ~Escapable, ~Copyable {
  let base: UnsafeMutableRawPointer
  let count: Int

  @lifetime(borrow p)
  init(_ p: UnsafeMutableRawPointer, _ c: Int) {
    self.base = p
    self.count = c
  }

  subscript(position: Int) -> Int {
    get {
      let offset = position * MemoryLayout<Int>.stride
      return base.loadUnaligned(fromByteOffset: offset, as: Int.self)
    }
    nonmutating set(newValue) {
      let offset = position * MemoryLayout<Int>.stride
      base.storeBytes(of: newValue, toByteOffset: offset,
                   as: Int.self)
    }
  }

  struct Iter: ~Escapable {
    var base: UnsafeMutableRawPointer
    var count: Int

    @lifetime(borrow base)
    init(base: UnsafeMutableRawPointer, count: Int) {
      self.base = base
      self.count = count
    }

    mutating func next() -> Int? {
      guard count > 0 else { return nil }
      count -= 1
      let n = base.load(as: Int.self)
      base = base + MemoryLayout<Int>.stride
      return n
    }
  }

  var iterator: Iter {
    @lifetime(copy self)
    get {
      let newIter = Iter(base: base, count: count)
      return _overrideLifetime(newIter, copying: self)
    }
  }
}

extension Array where Element == Int {
  // TODO: comment out dependsOn(scoped)
  mutating func mutspan() -> /* dependsOn(scoped self) */ MutableSpan {
    /* not the real implementation */
    let p = self.withUnsafeMutableBufferPointer { $0.baseAddress! }
    let span = MutableSpan(p, count)
    return _overrideLifetime(span, mutating: &self)
  }
}

struct NC : ~Copyable {
  let p: UnsafeMutableRawPointer
  let c: Int

  // Requires a mutable borrow.
  @lifetime(&self)
  mutating func getMutableSpan() -> MutableSpan {
    MutableSpan(p, c)
  }
}

func mbv_set_element(nc: inout NC, e: Int) {
  nc.getMutableSpan()[3] = e
}

func test_mutate_iterate() {
  var array = [1,2,3]
  // mutable 'array' access
  let mutatingSpan = array.mutspan()
  var iterator = mutatingSpan.iterator
  var i = 0
  while let n = iterator.next() {
    _ = n
    if i < mutatingSpan.count {
      mutatingSpan[i+1] = mutatingSpan[i]
      i += 1
    }
  }
  // end mutable 'array' access
}
