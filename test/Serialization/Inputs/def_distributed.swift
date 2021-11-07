import _Distributed

/// Use the existential wrapper as the default actor transport.
typealias DefaultActorTransport = AnyActorTransport

@available(SwiftStdlib 5.6, *)
public distributed actor DA {
  public distributed func doSomethingDistributed(param: String) async -> Int {
    return 0
  }
}
