import Distributed

typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem

@available(SwiftStdlib 5.6, *)
public distributed actor DA {
  public distributed func doSomethingDistributed(param: String) async -> Int {
    return 0
  }
}
