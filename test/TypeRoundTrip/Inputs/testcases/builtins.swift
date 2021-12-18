import RoundTrip

public func test() {
  roundTripType(Int.self)
  roundTripType(UInt.self)

  roundTripType(Float.self)
  roundTripType(Double.self)

  roundTripType(Int8.self)
  roundTripType(Int16.self)
  roundTripType(Int32.self)
  roundTripType(Int64.self)

  roundTripType(UInt8.self)
  roundTripType(UInt16.self)
  roundTripType(UInt32.self)
  roundTripType(UInt64.self)
}
