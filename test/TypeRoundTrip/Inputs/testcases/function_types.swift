import RoundTrip

class Class {}

let fn: (Int, Class, __owned Class, Any, inout Int) -> (Int, Class, Any) = {
  _, _, _, _, _ in fatalError()
}

let fn2: () throws -> () = {}

public func test() {
  roundTripType(type(of: fn))
  roundTripType(type(of: fn2))
}
