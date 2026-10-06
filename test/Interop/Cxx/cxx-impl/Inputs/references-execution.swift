import References

@unsafe @cxx @implementation
public func addOne(_ x: inout CInt) -> CInt {
  x += 1
  return x
}

@unsafe @cxx @implementation
public func swapRefs(_ a: inout CInt, _ b: inout CInt) {
  let t = a
  a = b
  b = t
}

@unsafe @cxx @implementation
public func readConstRef(_ x: CInt) -> CInt {
  return x * 10
}

let refStorage: UnsafeMutablePointer<CInt> = {
  let p = UnsafeMutablePointer<CInt>.allocate(capacity: 1)
  p.initialize(to: 7)
  return p
}()

let ptrSlot: UnsafeMutablePointer<UnsafeMutablePointer<CInt>> = {
  let p = UnsafeMutablePointer<UnsafeMutablePointer<CInt>>.allocate(capacity: 1)
  p.initialize(to: refStorage)
  return p
}()

@cxx @implementation
public func mutableRefReturn() -> UnsafeMutablePointer<CInt> {
  return refStorage
}

@cxx @implementation
public func constRefReturn() -> UnsafePointer<CInt> {
  return UnsafePointer(refStorage)
}

@cxx @implementation
public func refToPtrReturn() -> UnsafeMutablePointer<UnsafeMutablePointer<CInt>> {
  return ptrSlot
}

@unsafe @cxx @implementation
public func reseatPtr(_ p: inout UnsafeMutablePointer<CInt>?) {
  p = refStorage
}

@unsafe @cxx @implementation
public func refOverload(_ x: inout CInt) {
  x += 100
}

@unsafe @cxx @implementation
public func refOverload(_ x: CInt) {
  referencesGlobal = x
}

@cxx @implementation
public func refOverload(_ p: UnsafeMutablePointer<CInt>) {
  p.pointee += 1000
}

extension Accumulator {
  @unsafe @cxx @implementation
  public func addTo(_ target: inout CInt) -> CInt {
    target += total
    return target
  }
}

@unsafe @cxx @implementation
public func bumpTotal(_ acc: inout Accumulator) {
  acc.total += 1
}

@unsafe @cxx @implementation
public func readTotal(_ acc: Accumulator) -> CInt {
  return acc.total
}
