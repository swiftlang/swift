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

import TestsUtils

// Based on Joe Armstrong's task from the Programming Erlang book:
// > Write a ring benchmark.
// > Create N processes in a ring.
// > Send a message round the ring M times so that a total of N * M messages get sent.
// > Time how long this takes for different values of N and M.
//
// This benchmark measures how much time it takes to send messages around such "ring",
// it involves waking up multiple actors and as such also shows the efficiency of the scheduling.

public let ActorRing: [BenchmarkInfo] = [
  BenchmarkInfo(
    name: "ActorRing",
    runFunction: bench_ring,
    tags: [.actor],
    setUpFunction: { spawnRingActors(m: 100, n: 10) },
    tearDownFunction: { loopInit = nil }
  ),
]

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
  }

  func tell(_ token: Token) async {
    switch token.payload {
    case 1:
      print("Ring done: \(token)")
      // stop. this actor could "stop" now
      // ringStop.store(Timer().getTimeAsInt())
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

  init() {
    print("  Spawned, initial: ID:\(id)")
  }

  func spawnRingActors(m messages: Int, n actors: Int) {
    //  spawnStart.store(Timer().getTimeAsInt())

    var next: LoopMember = self
    for i in (1 ..< actors) {
      next = TokenLoopActor(id: i, next: next, token: Token(messages))
    }
    self.next = next
    //  spawnStop.store(Timer().getTimeAsInt())
  }


  func tell(_ token: Token) async {
    // print("Actor(\(id)) [\(token)]; token -> \(next)")
    await next?.tell(token)
  }
}

//private let mutex = _Mutex()

var loopInit: LoopInitActor? = nil

func spawnRingActors(m messages: Int, n actors: Int) {
  print("setup: start")
  loopInit = LoopInitActor()
  runAsyncAndBlock {
    await loopInit!.spawnRingActors(m: messages, n: actors)
  }
  print("setup: done")
}

// === -----------------------------------------------------------------------------------------------------------------

@inline(never)
func bench_ring(i: Int) {
  runAsyncAndBlock {
    print("run: go!!!")
    await loopInit!.tell(Token(100_000))
    print("run: done")
  }
}
