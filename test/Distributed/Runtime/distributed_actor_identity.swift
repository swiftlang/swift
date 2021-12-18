// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking -Xfrontend -enable-experimental-distributed -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import _Distributed

struct ActorAddress: ActorIdentity, CustomStringConvertible {
  let id: String
  var description: Swift.String {
    "ActorAddress(id: \(id))"
  }
}


func equality() {
  let a = ActorAddress(id: "a")
  let b = ActorAddress(id: "b")

  let anyA = AnyActorIdentity(a)
  let anyB = AnyActorIdentity(b)

  print("\(a == a)") // CHECK: true
  print("\(anyA == anyA)") // CHECK: true

  print("\(a != b)") // CHECK: true
  print("\(anyA != anyB)") // CHECK: true
}

func hash() {
  let a = ActorAddress(id: "a")
  let b = ActorAddress(id: "b")

  let anyA = AnyActorIdentity(a)
  let anyB = AnyActorIdentity(b)

  print("\(a.hashValue == a.hashValue)") // CHECK: true
  print("\(anyA.hashValue == anyA.hashValue)") // CHECK: true

  print("\(a.hashValue != b.hashValue)") // CHECK: true
  print("\(anyA.hashValue != anyB.hashValue)") // CHECK: true
}

@main struct Main {
  static func main() {
    equality()
    hash()
  }
}