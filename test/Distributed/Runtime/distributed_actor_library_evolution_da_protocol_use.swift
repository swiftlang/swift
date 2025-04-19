// REQUIRES: VENDOR=apple
// REQUIRES: concurrency
// REQUIRES: distributed

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift -Xfrontend -validate-tbd-against-ir=all -enable-library-evolution -target %target-cpu-apple-macosx13.0 -parse-as-library -emit-library -emit-module-path %t/Library.swiftmodule -module-name Library %t/library.swift -o %t/%target-library-name(Library)
// RUN: %target-build-swift -Xfrontend -validate-tbd-against-ir=all -target %target-cpu-apple-macosx13.0 -parse-as-library -lLibrary -module-name main -I %t -L %t %t/main.swift -o %t/a.out


// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

//--- library.swift
import Distributed

//public protocol NormalProtocol {
//  func NORMAL() async -> Int
//}

public protocol SimpleProtocol: DistributedActor
    where ActorSystem == LocalTestingDistributedActorSystem {

  // nonisolated override var id: ID { get } // comes from DistributedActor

  // Has to have a distributed method to fail
  distributed func test() -> Int
}

//--- main.swift
import Distributed
import Library

//actor NormalActor: NormalProtocol {
//  func NORMAL() async -> Int { 1 }
//}

public distributed actor SimpleActor: SimpleProtocol {
  public distributed func test() -> Int { 1 }
}

// Passes
public func makeFromPass<Act: DistributedActor>(_ act: Act) {
  print(act.id)
}

// Fails
public func makeFromFail<Act: SimpleProtocol>(_ act: Act) async {
  print(act.id)
  try! await print(act.test())
}

@main
struct TestSwiftFrameworkTests {
  static func main() async {
    let system = LocalTestingDistributedActorSystem()

    // let norm = NormalActor()

    let simpleActor = SimpleActor(actorSystem: system)
//    makeFromPass(simpleActor)

    await makeFromFail(simpleActor)
  }
}