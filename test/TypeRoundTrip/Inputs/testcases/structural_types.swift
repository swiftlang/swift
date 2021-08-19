import RoundTrip

func ownedString(_ s: __owned String) -> () {
}

func variadic(_ ints: Int...) -> () {
}

public func test() {
  roundTripType((() -> ()).self)
  roundTripType(((inout String) -> ()).self)
  roundTripType(((Int, Float) -> ()).self)
  roundTripType(((inout Int, Float) -> ()).self)
  roundTripType(((inout Int, inout Float) -> ()).self)
  roundTripType(((Int, inout Float) -> ()).self)
  roundTripType(((Int, inout String, Float) -> ()).self)
  roundTripType(((inout Int, String, inout Float, Double) -> ()).self)
  roundTripType(((String, Int, Double, Float) -> ()).self)
  roundTripType(((Int, Float) -> ()).self)
  roundTripType(((Int, Float, Int) -> ()).self)
  roundTripType((Int.Type, x: Float, Int).self)
  roundTripType((x: Int, Float, y: Int.Type).self)
  roundTripType(((@escaping () -> ()) -> ()).self)
  roundTripType(Array<@convention(c) () -> ()>.self)

  // @convention(block) requires Objective-C support
  #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
  roundTripType(Array<(@escaping @convention(block) () -> (), @convention(block) () -> ()) -> ()>.self)
  #endif

  roundTripType(Int.Type.self)
  roundTripType(((inout String) -> ()).Type.self)
  roundTripType(((Int, Float) -> ()).Type.self)
  roundTripType(((inout Int, Float) -> ()).Type.self)
  roundTripType(((inout Int, inout Float) -> ()).Type.self)
  roundTripType(((Int, inout Float) -> ()).Type.self)
  roundTripType(((Int, inout String, Float) -> ()).Type.self)
  roundTripType(((inout Int, String, inout Float, Double) -> ()).Type.self)
  roundTripType(((String, Int, Double, Float) -> ()).Type.self)
  roundTripType(((Int, Float) -> ()).Type.self)
  roundTripType(((Int, Float, Int) -> ()).Type.self)
  roundTripType((Int.Type, x: Float, Int).Type.self)
  roundTripType((x: Int, Float, y: Int.Type).Type.self)
  roundTripType(((@escaping () -> ()) -> ()).Type.self)
  roundTripType(Array<@convention(c) () -> ()>.Type.self)

  // @convention(block) requires Objective-C support
  #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
  roundTripType(Array<(@escaping @convention(block) () -> (), @convention(block) () -> ()) -> ()>.Type.self)
  #endif

  // rdar://81587763: [SR-15025]: Function type syntax doesn't accept variadics
  // or __owned
  //
  //roundTripType(((__owned String) -> ()).self)
  //roundTripType(((__owned String) -> ()).Type.self)
  roundTripType(type(of: ownedString))
  roundTripType(type(of:type(of: ownedString)))
  //roundTripType(((Int...) -> ()).self)
  //roundTripType(((Int...) -> ()).Type.self)
  roundTripType(type(of: variadic))
  roundTripType(type(of:type(of: variadic)))
}
