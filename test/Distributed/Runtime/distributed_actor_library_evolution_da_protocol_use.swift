// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: concurrency
// REQUIRES: distributed
// UNSUPPORTED: use_os_stdlib

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift -Xfrontend -validate-tbd-against-ir=all -enable-library-evolution -target %target-cpu-apple-macosx13.0 -parse-as-library -emit-library -emit-module-path %t/Library.swiftmodule -module-name Library %t/library.swift -o %t/%target-library-name(Library)
// RUN: %target-build-swift -Xfrontend -validate-tbd-against-ir=all -target %target-cpu-apple-macosx13.0 -parse-as-library -lLibrary -module-name main -I %t -L %t %t/main.swift -o %t/a.out

// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

//--- library.swift
import Distributed

public protocol SimpleProtocol: DistributedActor
    where ActorSystem == LocalTestingDistributedActorSystem {

  // nonisolated override var id: ID { get } // comes from DistributedActor

  // Has to have a distributed method to fail
  distributed func test() -> Int
}

//--- main.swift
import Distributed
import Library

public distributed actor SimpleActor: SimpleProtocol {
  public distributed func test() -> Int {
    print("SimpleActor.test")
    return 1
  }
}

public func makeFromFail<Act: SimpleProtocol>(_ act: Act) async {
  print(act.id)
  try! await print("act.test() = \(act.test())")
  // CHECK: SimpleActor.test
  // CHECK: act.test() = 1
}

@main
struct TestSwiftFrameworkTests {
  static func main() async {
    let system = LocalTestingDistributedActorSystem()

    let simpleActor = SimpleActor(actorSystem: system)
    await makeFromFail(simpleActor)
  }
}