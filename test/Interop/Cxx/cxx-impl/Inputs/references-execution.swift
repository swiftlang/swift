import References

@cxx @implementation
public func addOne(_ x: UnsafeMutablePointer<CInt>) -> CInt {
  x.pointee += 1
  return x.pointee
}

@cxx @implementation
public func swapRefs(_ a: UnsafeMutablePointer<CInt>, _ b: UnsafeMutablePointer<CInt>) {
  let t = a.pointee
  a.pointee = b.pointee
  b.pointee = t
}

@cxx @implementation
public func observe(_ a: UnsafeMutablePointer<CInt>, _ b: UnsafeMutablePointer<CInt>) -> CInt {
  a.pointee = 1
  b.pointee = 2
  return a.pointee
}

@cxx @implementation
public func observeGlobal(_ x: UnsafeMutablePointer<CInt>) -> CInt {
  referencesGlobal = 5
  x.pointee = 7
  return referencesGlobal
}

@cxx @implementation
public func readConstRef(_ x: UnsafePointer<CInt>) -> CInt {
  return x.pointee * 10
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

@cxx @implementation
public func reseatPtr(_ p: UnsafeMutablePointer<UnsafeMutablePointer<CInt>?>) {
  p.pointee = refStorage
}

@cxx @implementation
public func refOverload(_ x: UnsafeMutablePointer<CInt>) {
  x.pointee += 100
}

@cxx @implementation
public func refOverload(_ x: UnsafePointer<CInt>) {
  referencesGlobal = x.pointee
}

extension Accumulator {
  @cxx @implementation
  public func addTo(_ target: UnsafeMutablePointer<CInt>) -> CInt {
    target.pointee += total
    return target.pointee
  }
}

@cxx @implementation
public func bumpTotal(_ acc: UnsafeMutablePointer<Accumulator>) { acc.pointee.total += 1 }

@cxx @implementation
public func readTotal(_ acc: UnsafePointer<Accumulator>) -> CInt { return acc.pointee.total }

@cxx @implementation
public func identityRef(_ acc: UnsafeMutablePointer<Accumulator>) -> UnsafeMutablePointer<Accumulator> {
  return acc
}

@cxx @implementation
public func holderMatches(_ h: UnsafePointer<PointerHolder>, _ p: UnsafeMutablePointer<CInt>) -> Bool {
  return h.pointee == p
}
