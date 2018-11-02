@_cdecl("test")
public func test() -> UInt {
  return unsafeBitCast(c, to: UInt.self)
}

class C {
  let x = "123"
  let y = 456
}

let c = C()
