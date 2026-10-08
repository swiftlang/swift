//===--- DistributedThunkHop.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Cost of the executor hop a distributed thunk performs before entering `remoteCall`.
//
// Before adoption of nonisolated(nonsending) remote calls would pessimistically
// have to hop off from the caller to the global pool, causing un-necessary hops.
//
// Since 6.5:
// We allow opting into end-to-end nonisolated(nonsending), which avoids these hops.
//
// Reading the results: the harness reports time per unit of `n`, and every case
// here performs `callsPerIteration` remote calls per unit. Divide the reported
// figure by that constant to get the cost of a single call round trip. All six
// cases use the same constant, so the raw numbers are directly comparable

import TestsUtils

#if canImport(Distributed)
import Distributed
#endif

public let benchmarks: [BenchmarkInfo] = {
#if canImport(Distributed)
  guard #available(macOS 15, iOS 18, tvOS 18, watchOS 11, *) else { return [] }
  return [
    BenchmarkInfo(
      name: "DistributedThunkHop.Actor.Hopping",
      runFunction: { await runActorCallerHopping($0) },
      tags: [.concurrency, .distributed]),
    BenchmarkInfo(
      name: "DistributedThunkHop.Actor.nonsending",
      runFunction: { await runActorCallerNonsending($0) },
      tags: [.concurrency, .distributed]),
    BenchmarkInfo(
      name: "DistributedThunkHop.MainActor.Hopping",
      runFunction: { await runMainHopping($0) },
      tags: [.concurrency, .distributed]),
    BenchmarkInfo(
      name: "DistributedThunkHop.MainActor.nonsending",
      runFunction: { await runMainNonsending($0) },
      tags: [.concurrency, .distributed]),
    BenchmarkInfo(
      name: "DistributedThunkHop.Concurrent.Hopping",
      runFunction: { await runConcurrentHopping($0) },
      tags: [.concurrency, .distributed]),
    BenchmarkInfo(
      name: "DistributedThunkHop.Concurrent.nonsending",
      runFunction: { await runConcurrentNonsending($0) },
      tags: [.concurrency, .distributed]),
  ]
#else
  return []
#endif
}()

#if canImport(Distributed)

// ==== ------------------------------------------------------------------------
// MARK: Systems

@available(macOS 15, iOS 18, tvOS 18, watchOS 11, *)
final class HoppingSystem: DistributedActorSystem, @unchecked Sendable {
  typealias ActorID = String
  typealias InvocationEncoder = NopEncoder
  typealias InvocationDecoder = NopDecoder
  typealias SerializationRequirement = Codable
  typealias ResultHandler = NopResultHandler

  func resolve<Act>(id: ActorID, as t: Act.Type) throws -> Act?
    where Act: DistributedActor, Act.ID == ActorID { nil }
  func assignID<Act>(_ t: Act.Type) -> ActorID
    where Act: DistributedActor, Act.ID == ActorID { "id" }
  func actorReady<Act>(_ a: Act) where Act: DistributedActor, Act.ID == ActorID {}
  func resignID(_ id: ActorID) {}
  func makeInvocationEncoder() -> InvocationEncoder { NopEncoder() }

  func remoteCall<Act, Err, Res>(
      on actor: Act, target: RemoteCallTarget,
      invocation: inout InvocationEncoder,
      throwing: Err.Type, returning: Res.Type) async throws -> Res
    where Act: DistributedActor, Act.ID == ActorID, Err: Error, Res: Codable {
    return 0 as! Res
  }
  func remoteCallVoid<Act, Err>(
      on actor: Act, target: RemoteCallTarget,
      invocation: inout InvocationEncoder, throwing: Err.Type) async throws
    where Act: DistributedActor, Act.ID == ActorID, Err: Error {}
}

@available(macOS 15, iOS 18, tvOS 18, watchOS 11, *)
final class NonsendingSystem: DistributedActorSystem, @unchecked Sendable {
  typealias ActorID = String
  typealias InvocationEncoder = NopEncoder
  typealias InvocationDecoder = NopDecoder
  typealias SerializationRequirement = Codable
  typealias ResultHandler = NopResultHandler

  func resolve<Act>(id: ActorID, as t: Act.Type) throws -> Act?
    where Act: DistributedActor, Act.ID == ActorID { nil }
  func assignID<Act>(_ t: Act.Type) -> ActorID
    where Act: DistributedActor, Act.ID == ActorID { "id" }
  func actorReady<Act>(_ a: Act) where Act: DistributedActor, Act.ID == ActorID {}
  func resignID(_ id: ActorID) {}
  func makeInvocationEncoder() -> InvocationEncoder { NopEncoder() }

  nonisolated(nonsending)
  func remoteCall<Act, Err, Res>(
      on actor: Act, target: RemoteCallTarget,
      invocation: inout InvocationEncoder,
      throwing: Err.Type, returning: Res.Type) async throws -> Res
    where Act: DistributedActor, Act.ID == ActorID, Err: Error, Res: Codable {
    return 0 as! Res
  }
  nonisolated(nonsending)
  func remoteCallVoid<Act, Err>(
      on actor: Act, target: RemoteCallTarget,
      invocation: inout InvocationEncoder, throwing: Err.Type) async throws
    where Act: DistributedActor, Act.ID == ActorID, Err: Error {}
}

struct NopEncoder: DistributedTargetInvocationEncoder {
  typealias SerializationRequirement = Codable
  mutating func recordGenericSubstitution<T>(_ t: T.Type) throws {}
  mutating func recordArgument<V: Codable>(_ a: RemoteCallArgument<V>) throws {}
  mutating func recordReturnType<R: Codable>(_ t: R.Type) throws {}
  mutating func recordErrorType<E: Error>(_ t: E.Type) throws {}
  mutating func doneRecording() throws {}
}
struct NopDecoder: DistributedTargetInvocationDecoder {
  typealias SerializationRequirement = Codable
  mutating func decodeGenericSubstitutions() throws -> [Any.Type] { [] }
  mutating func decodeNextArgument<A: Codable>() throws -> A { fatalError() }
  mutating func decodeReturnType() throws -> Any.Type? { nil }
  mutating func decodeErrorType() throws -> Any.Type? { nil }
}
struct NopResultHandler: DistributedTargetInvocationResultHandler {
  typealias SerializationRequirement = Codable
  func onReturn<S: Codable>(value: S) async throws {}
  func onReturnVoid() async throws {}
  func onThrow<E: Error>(error: E) async throws {}
}

// ==== ------------------------------------------------------------------------
// MARK: Targets

@available(macOS 15, iOS 18, tvOS 18, watchOS 11, *)
distributed actor Hopper {
  typealias ActorSystem = HoppingSystem
  distributed func poke() -> Int { 0 }
}

@available(macOS 15, iOS 18, tvOS 18, watchOS 11, *)
distributed actor LessHopper {
  typealias ActorSystem = NonsendingSystem
  distributed func poke() -> Int { 0 }
}

// ==== ------------------------------------------------------------------------
// MARK: Callers

@available(macOS 15, iOS 18, tvOS 18, watchOS 11, *)
actor ActorCaller {
  func hopping(_ n: Int, _ a: Hopper) async {
    var acc = 0
    for _ in 0..<n { acc += (try? await a.poke()) ?? 1 }
    check(acc == 0)
  }
  func nonsending(_ n: Int, _ a: LessHopper) async {
    var acc = 0
    for _ in 0..<n { acc += (try? await a.poke()) ?? 1 }
    check(acc == 0)
  }
}

@MainActor
@available(macOS 15, iOS 18, tvOS 18, watchOS 11, *)
func mainHopping(_ n: Int, _ a: Hopper) async {
  var acc = 0
  for _ in 0..<n { acc += (try? await a.poke()) ?? 1 }
  check(acc == 0)
}

@MainActor
@available(macOS 15, iOS 18, tvOS 18, watchOS 11, *)
func mainNonsending(_ n: Int, _ a: LessHopper) async {
  var acc = 0
  for _ in 0..<n { acc += (try? await a.poke()) ?? 1 }
  check(acc == 0)
}

// A `@concurrent` nonisolated caller already runs on the generic executor and
// has no isolation to inherit, so neither thunk flavour performs a real
// executor switch: the hopping thunk's `hop_to_executor` to the generic
// executor resolves to a no-op, and the caller-isolated thunk is handed no
// actor at all.
//
// The pair is therefore a control that isolates what remains once the *switch*
// is free. Measured, that residue is still about 2x (roughly 6ns vs 3ns per
// call): the hopping thunk must be a separate `@concurrent` async function, so
// each call pays for its own async frame plus the `swift_task_switch` check
// that decides the hop is unnecessary, whereas the caller-isolated thunk can
// simply inline into the caller. Compare against the MainActor pair, where the
// switch is a genuine cross-executor transition and dominates everything else

@concurrent
@available(macOS 15, iOS 18, tvOS 18, watchOS 11, *)
func concurrentHopping(_ n: Int, _ a: Hopper) async {
  var acc = 0
  for _ in 0..<n { acc += (try? await a.poke()) ?? 1 }
  check(acc == 0)
}

@concurrent
@available(macOS 15, iOS 18, tvOS 18, watchOS 11, *)
func concurrentNonsending(_ n: Int, _ a: LessHopper) async {
  var acc = 0
  for _ in 0..<n { acc += (try? await a.poke()) ?? 1 }
  check(acc == 0)
}

// ==== ------------------------------------------------------------------------
// MARK: Entry points

// Remote calls performed per unit of `n`, identical for every case so that the
// reported figures can be compared directly and divided by a single constant to
// obtain per-call cost.
//
// The value has to be large enough that the fixed per-sample cost amortizes
// away. Entering a `@MainActor` timed function costs one real executor hop, and
// the harness divides each sample by `n`, so that hop shows up as an inflated
// per-call figure at small scales: measured with 20 calls per unit, the cheapest
// case read 0.340us at `--num-iters=50` but 0.064us at `--num-iters=4000`, all
// of the difference being setup rather than call cost. It also has to stay small
// enough that the most expensive case, hopping off the main actor at ~9us per
// call, does not blow past the suite's ~1ms per workload guideline
let callsPerIteration = 100

@available(macOS 15, iOS 18, tvOS 18, watchOS 11, *)
func runActorCallerHopping(_ n: Int) async {
  let a = try! Hopper.resolve(id: "x", using: HoppingSystem())
  await ActorCaller().hopping(n * callsPerIteration, a)
}

@available(macOS 15, iOS 18, tvOS 18, watchOS 11, *)
func runActorCallerNonsending(_ n: Int) async {
  let a = try! LessHopper.resolve(id: "x", using: NonsendingSystem())
  await ActorCaller().nonsending(n * callsPerIteration, a)
}

@available(macOS 15, iOS 18, tvOS 18, watchOS 11, *)
func runMainHopping(_ n: Int) async {
  let a = try! Hopper.resolve(id: "x", using: HoppingSystem())
  await mainHopping(n * callsPerIteration, a)
}

@available(macOS 15, iOS 18, tvOS 18, watchOS 11, *)
func runMainNonsending(_ n: Int) async {
  let a = try! LessHopper.resolve(id: "x", using: NonsendingSystem())
  await mainNonsending(n * callsPerIteration, a)
}

@available(macOS 15, iOS 18, tvOS 18, watchOS 11, *)
func runConcurrentHopping(_ n: Int) async {
  let a = try! Hopper.resolve(id: "x", using: HoppingSystem())
  await concurrentHopping(n * callsPerIteration, a)
}

@available(macOS 15, iOS 18, tvOS 18, watchOS 11, *)
func runConcurrentNonsending(_ n: Int) async {
  let a = try! LessHopper.resolve(id: "x", using: NonsendingSystem())
  await concurrentNonsending(n * callsPerIteration, a)
}

#endif // canImport(Distributed)
