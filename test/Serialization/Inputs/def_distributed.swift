import _Distributed

@available(SwiftStdlib 5.5, *)
public distributed actor DA {
  public distributed func doSomethingDistributed(param: String) async -> Int {
    return 0
  }
}
