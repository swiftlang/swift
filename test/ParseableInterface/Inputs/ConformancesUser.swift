import Conformances

func testGeneric<T: MyProto>(_: T.Type) -> Int {
  var impl = T.init()
  impl.method()
  impl.prop = 0
  return impl[0]
}

public func testFull() -> Int {
  return testGeneric(FullStructImpl.self)
}

public func testOpaque() -> Int {
  return testGeneric(OpaqueStructImpl.self)
}

public func testResilient() -> Int {
  return testGeneric(ResilientStructImpl.self)
}
