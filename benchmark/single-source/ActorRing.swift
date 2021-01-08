//===--- ActorRing.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

//import TestsUtils

// Based on Joe Armstrong's task from the Programming Erlang book:
// > Write a ring benchmark.
// > Create N processes in a ring.
// > Send a message round the ring M times so that a total of N * M messages get sent.
// > Time how long this takes for different values of N and M.
//
// This benchmark measures how much time it takes to send messages around such "ring",
// it involves waking up multiple actors and as such also shows the efficiency of the scheduling.

//public let RingTests = [
//  BenchmarkInfo(
//    name: "RingBenchmarks.bench_ring_m100_n10",
//    runFunction: { bench_ring() },
//    tags: [.actor],
//    setUpFunction: { spawnRingActors(m: 100, n: 10) },
//    tearDownFunction: { loopInit = nil }
//  ),
//]

// === -------------------------------------------------------------------------
//private let q = LinkedBlockingQueue<Int>()
//
//private let spawnStart = Atomic<UInt64>(value: 0)
//private let spawnStop = Atomic<UInt64>(value: 0)
//
//private let ringStart = Atomic<UInt64>(value: 0)
//private let ringStop = Atomic<UInt64>(value: 0)

// === -------------------------------------------------------------------------

struct Token {
  let payload: Int

  init(_ payload: Int) {
    self.payload = payload
  }
}

protocol LoopMember {
  // var id: Int { get }
  func tell(_ token: Token) async
}

actor class TokenLoopActor: LoopMember {
  let id: Int
  let next: LoopMember
  let token: Token

  init(id: Int, next: LoopMember, token: Token) {
    self.id = id
    self.next = next
    self.token = token

//    if id == 1 {
//      // I am the leader and shall create the ring
//      self.spawnActorRing()
//    }
  }

  func tell(_ token: Token) async {
    switch token.payload {
    case 1:
      //      ringStop.store(Timer().getTimeAsInt())
      // stop. this actor could "stop" now
      // q.enqueue(0) // done
      break;

    default:
      await next.tell(Token(token.payload - 1))
    }
  }
}

actor class LoopInitActor: LoopMember {
  let id = 0
  var next: LoopMember?

  func spawnRingActors(m messages: Int, n actors: Int) {
    // TIME spawning
    //  spawnStart.store(Timer().getTimeAsInt())

    var next: LoopMember = self
    for i in (1 ..< actors).reversed() {
      next = TokenLoopActor(id: i, next: next, token: Token(messages))
    }
    //  spawnStop.store(Timer().getTimeAsInt())
  }


  func tell(_ token: Token) async {
    // print("START RING SEND... \(Timer().getTime())")

    // print("Send \(m) \(context.myself.path.name) >>> \(loopRef.path.name)")
    await next?.tell(token)

    // self.token = token
    // return loopMember(id: 1, next: loopRef, token: token)
  }
}

//private let mutex = _Mutex()

var loopInit: LoopInitActor! = nil

func spawnRingActors(m messages: Int, n actors: Int) {
  loopInit = LoopInitActor()
  runAsyncAndBlock {
    await loopInit.spawnRingActors(m: messages, n: actors)
  }
}

// === -----------------------------------------------------------------------------------------------------------------

//@inline(never)
//func bench_ring() {
//  //  ringStart.store(Timer().getTimeAsInt())
//  loopInit.tell(Token(100_000))
//
//  //  _ = q.poll(.seconds(20))
//  //  print("    Spawning           : \((spawnStop.load() - spawnStart.load()).milliseconds) ms")
//  //  print("    Sending around Ring: \((ringStop.load() - ringStart.load()).milliseconds) ms")
//}
