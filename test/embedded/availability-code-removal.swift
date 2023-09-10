// RUN: %target-swift-frontend -emit-ir %s -parse-stdlib -enable-experimental-feature Embedded

public protocol Player {}
struct Concrete: Player {}

@_unavailableInEmbedded
public func test() -> any Player {
  Concrete() // no error because we're in unavailable-in-embedded context
}
