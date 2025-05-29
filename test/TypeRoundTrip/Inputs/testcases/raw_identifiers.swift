import RoundTrip

struct `Raw Identifier` {
  struct `Nested.Type` {} 
  struct `Nested<Generic>`<T> {} 
}

public func test() {
  roundTripType(`Raw Identifier`.self)
  roundTripType(`Raw Identifier`.`Nested.Type`.self)
  roundTripType(`Raw Identifier`.`Nested<Generic>`<Int>.self)
}
