import RoundTrip

public func test() {
  roundTripType(Array<(Int) async -> ()>.self)
  roundTripType(Array<(Int) async throws -> ()>.self)
}
