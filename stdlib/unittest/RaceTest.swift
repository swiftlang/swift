//===--- RaceTest.swift ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This file implements support for race tests.
///
/// Race test harness executes the given operation in multiple threads over a
/// set of shared data, trying to ensure that executions overlap in time.
///
/// The name "race test" does not imply that the race actually happens in the
/// harness or in the operation being tested.  The harness contains all the
/// necessary synchronization for its own data, and for publishing test data to
/// threads.  But if the operation under test is, in fact, racy, it should be
/// easier to discover the bug in this environment.
///
/// Every execution of a race test is called a trial.  During a single trial
/// the operation under test is executed multiple times in each thread over
/// different data items (`RaceData` instances).  Different threads process
/// data in different order.  Choosing an appropriate balance between the
/// number of threads and data items, the harness uses the birthday paradox to
/// increase the probability of "collisions" between threads.
///
/// After performing the operation under test, the thread should observe the
/// data in a test-dependent way to detect if presence of other concurrent
/// actions disturbed the result.  The observation should be as short as
/// possible, and the results should be returned as `Observation`.  Evaluation
/// (cross-checking) of observations is deferred until the end of the trial.
///
//===----------------------------------------------------------------------===//

import SwiftUnstable
import SwiftUnstablePthreadExtras
#if os(OSX) || os(iOS)
import Darwin
#elseif os(Linux)
import Glibc
#endif

#if _runtime(_ObjC)
import ObjectiveC
#else
func autoreleasepool(@noescape code: () -> ()) {
  // Native runtime does not have autorelease pools.  Execute the code
  // directly.
  code()
}
#endif

/// Race tests that need a fresh set of data for every trial should implement
/// this protocol.
///
/// All racing threads execute the same operation, `thread1`.
///
/// Types conforming to this protocol should be structs.  (The type
/// should be a struct to reduce unnecessary reference counting during
/// the test.)  The types should be stateless.
public protocol RaceTestWithPerTrialDataType {

  /// Input for threads.
  ///
  /// This type should be a class.  (The harness will not pass struct instances
  /// between threads correctly.)
  typealias RaceData //: AnyObject
  // FIXME: can not add a class constraint because of:
  // <rdar://problem/18305834> "AnyObject" superclass on an associated type
  // causes a runtime crash

  /// Type of thread-local data.
  ///
  /// Thread-local data is newly created for every trial.
  typealias ThreadLocalData

  /// Results of the observation made after performing an operation.
  typealias Observation

  init()

  /// Creates a fresh instance of `RaceData`.
  func makeRaceData() -> RaceData

  /// Creates a fresh instance of `ThreadLocalData`.
  func makeThreadLocalData() -> ThreadLocalData

  /// Performs the operation under test and makes an observation.
  func thread1(
    raceData: RaceData, inout _ threadLocalData: ThreadLocalData) -> Observation

  /// Evaluates the observations made by all threads for a particular instance
  /// of `RaceData`.
  func evaluateObservations<
    S : SinkType where S.Element == RaceTestObservationEvaluation
  >(observations: _UnitTestArray<Observation>, inout _ sink: S)
}

/// The result of evaluating observations.
///
/// Case payloads can carry test-specific data.  Results will be grouped
/// according to it.
public enum RaceTestObservationEvaluation : Equatable, Printable {
  /// Normal 'pass'.
  case Pass

  /// An unusual 'pass'.
  case PassInteresting(String)

  /// A failure.
  case Failure
  case FailureInteresting(String)

  public var description: String {
    switch self {
    case .Pass:
      return "Pass"

    case .PassInteresting(let s):
      return "Pass(\(s))"

    case .Failure:
      return "Failure"

    case .FailureInteresting(let s):
      return "Failure(\(s))"
    }
  }
}

public func == (
  lhs: RaceTestObservationEvaluation, rhs: RaceTestObservationEvaluation
) -> Bool {
  switch (lhs, rhs) {
  case (.Pass, .Pass),
       (.Failure, .Failure):
    return true

  case (.PassInteresting(let s1), .PassInteresting(let s2)):
    return s1 == s2

  default:
    return false
  }
}

/// An observation result that consists of one `UWord`.
public struct Observation1UWord : Equatable, Printable {
  public var uw1: UWord

  public init(_ uw1: UWord) {
    self.uw1 = uw1
  }

  public var description: String {
    return "(\(uw1))"
  }
}

public func == (lhs: Observation1UWord, rhs: Observation1UWord) -> Bool {
  return lhs.uw1 == rhs.uw1
}

/// An observation result that consists of four `UWord`\ s.
public struct Observation4UWord : Equatable, Printable {
  public var uw1: UWord
  public var uw2: UWord
  public var uw3: UWord
  public var uw4: UWord

  public init(_ uw1: UWord, _ uw2: UWord, _ uw3: UWord, _ uw4: UWord) {
    self.uw1 = uw1
    self.uw2 = uw2
    self.uw3 = uw3
    self.uw4 = uw4
  }

  public var description: String {
    return "(\(uw1), \(uw2), \(uw3), \(uw4))"
  }
}

public func == (lhs: Observation4UWord, rhs: Observation4UWord) -> Bool {
  return
    lhs.uw1 == rhs.uw1 &&
    lhs.uw2 == rhs.uw2 &&
    lhs.uw3 == rhs.uw3 &&
    lhs.uw4 == rhs.uw4
}

/// An observation result that consists of four `Word`\ s.
public struct Observation4Word : Equatable, Printable {
  public var w1: Word
  public var w2: Word
  public var w3: Word
  public var w4: Word

  public init(_ w1: Word, _ w2: Word, _ w3: Word, _ w4: Word) {
    self.w1 = w1
    self.w2 = w2
    self.w3 = w3
    self.w4 = w4
  }

  public var description: String {
    return "(\(w1), \(w2), \(w3), \(w4))"
  }
}

public func == (lhs: Observation4Word, rhs: Observation4Word) -> Bool {
  return
    lhs.w1 == rhs.w1 &&
    lhs.w2 == rhs.w2 &&
    lhs.w3 == rhs.w3 &&
    lhs.w4 == rhs.w4
}

/// An observation result that consists of five `Word`\ s.
public struct Observation5Word : Equatable, Printable {
  public var w1: Word
  public var w2: Word
  public var w3: Word
  public var w4: Word
  public var w5: Word

  public init(_ w1: Word, _ w2: Word, _ w3: Word, _ w4: Word, _ w5: Word) {
    self.w1 = w1
    self.w2 = w2
    self.w3 = w3
    self.w4 = w4
    self.w5 = w5
  }

  public var description: String {
    return "(\(w1), \(w2), \(w3), \(w4), \(w5))"
  }
}

public func == (lhs: Observation5Word, rhs: Observation5Word) -> Bool {
  return
    lhs.w1 == rhs.w1 &&
    lhs.w2 == rhs.w2 &&
    lhs.w3 == rhs.w3 &&
    lhs.w4 == rhs.w4 &&
    lhs.w5 == rhs.w5
}

/// An observation result that consists of nine `Word`\ s.
public struct Observation9Word : Equatable, Printable {
  public var w1: Word
  public var w2: Word
  public var w3: Word
  public var w4: Word
  public var w5: Word
  public var w6: Word
  public var w7: Word
  public var w8: Word
  public var w9: Word

  public init(
    _ w1: Word, _ w2: Word, _ w3: Word, _ w4: Word,
    _ w5: Word, _ w6: Word, _ w7: Word, _ w8: Word,
    _ w9: Word
  ) {
    self.w1 = w1
    self.w2 = w2
    self.w3 = w3
    self.w4 = w4
    self.w5 = w5
    self.w6 = w6
    self.w7 = w7
    self.w8 = w8
    self.w9 = w9
  }

  public var description: String {
    return "(\(w1), \(w2), \(w3), \(w4), \(w5), \(w6), \(w7), \(w8), \(w9))"
  }
}

public func == (lhs: Observation9Word, rhs: Observation9Word) -> Bool {
  return
    lhs.w1 == rhs.w1 &&
    lhs.w2 == rhs.w2 &&
    lhs.w3 == rhs.w3 &&
    lhs.w4 == rhs.w4 &&
    lhs.w5 == rhs.w5 &&
    lhs.w6 == rhs.w6 &&
    lhs.w7 == rhs.w7 &&
    lhs.w8 == rhs.w8 &&
    lhs.w9 == rhs.w9
}

/// A helper that is useful to implement
/// `RaceTestWithPerTrialDataType.evaluateObservations()` in race tests.
public func evaluateObservationsAllEqual<T : Equatable>(observations: _UnitTestArray<T>)
  -> RaceTestObservationEvaluation {
  let first = observations.first!
  for x in observations {
    if x != first {
      return .Failure
    }
  }
  return .Pass
}

struct _RaceTestAggregatedEvaluations : Printable {
  var passCount: Int = 0
  var passInterestingCount = [String: Int]()
  var failureCount: Int = 0
  var failureInterestingCount = [String: Int]()

  init() {}

  mutating func addEvaluation(evaluation: RaceTestObservationEvaluation) {
    switch evaluation {
    case .Pass:
      ++passCount

    case .PassInteresting(let s):
      if passInterestingCount[s] == nil {
        passInterestingCount[s] = 0
      }
      passInterestingCount[s] = passInterestingCount[s]! + 1

    case .Failure:
      ++failureCount

    case .FailureInteresting(let s):
      if failureInterestingCount[s] == nil {
        failureInterestingCount[s] = 0
      }
      failureInterestingCount[s] = failureInterestingCount[s]! + 1
    }
  }

  var isFailed: Bool {
    return failureCount != 0 || !failureInterestingCount.isEmpty
  }

  var description: String {
    var result = ""
    result += "Pass: \(passCount) times\n"
    for desc in sorted(passInterestingCount.keys) {
      let count = passInterestingCount[desc]!
      result += "Pass \(desc): \(count) times\n"
    }
    result += "Failure: \(failureCount) times\n"
    for desc in sorted(failureInterestingCount.keys) {
      let count = failureInterestingCount[desc]!
      result += "Failure \(desc): \(count) times\n"
    }
    return result
  }
}

// FIXME: protect this class against false sharing.
class _RaceTestWorkerState<RT : RaceTestWithPerTrialDataType> {
  // FIXME: protect every element of 'raceData' against false sharing.
  var raceData: _UnitTestArray<RT.RaceData> = []
  var raceDataShuffle: _UnitTestArray<Int> = []
  var observations: _UnitTestArray<RT.Observation> = []
}

class _RaceTestSharedState<RT : RaceTestWithPerTrialDataType> {
  var racingThreadCount: Int

  var trialBarrier: _stdlib_Barrier
  var trialSpinBarrier: _stdlib_AtomicInt = _stdlib_AtomicInt()

  var raceData: _UnitTestArray<RT.RaceData> = []
  var workerStates: _UnitTestArray<_RaceTestWorkerState<RT>> = []
  var aggregatedEvaluations: _RaceTestAggregatedEvaluations =
    _RaceTestAggregatedEvaluations()

  init(racingThreadCount: Int) {
    self.racingThreadCount = racingThreadCount
    self.trialBarrier = _stdlib_Barrier(threadCount: racingThreadCount + 1)

    self.workerStates.reserveCapacity(racingThreadCount)
    for i in 0..<racingThreadCount {
      self.workerStates.append(_RaceTestWorkerState<RT>())
    }
  }
}

func _masterThreadOneTrial<RT : RaceTestWithPerTrialDataType>(
  sharedState: _RaceTestSharedState<RT>
) {
  let racingThreadCount = sharedState.racingThreadCount
  let raceDataCount = racingThreadCount * racingThreadCount
  let rt = RT()

  sharedState.raceData.removeAll(keepCapacity: true)
  sharedState.raceData.extend(
    lazy(0..<raceDataCount).map { i in rt.makeRaceData() })

  let identityShuffle = _UnitTestArray(0..<sharedState.raceData.count)
  sharedState.workerStates.removeAll(keepCapacity: true)
  sharedState.workerStates.extend(
    lazy(0..<racingThreadCount).map {
      i in
      let workerState = _RaceTestWorkerState<RT>()

      // Shuffle the data so that threads process it in different order.
      let shuffle = randomShuffle(identityShuffle)
      workerState.raceData = scatter(sharedState.raceData, shuffle)
      workerState.raceDataShuffle = shuffle

      workerState.observations = []
      workerState.observations.reserveCapacity(sharedState.raceData.count)

      return workerState
    })

  sharedState.trialSpinBarrier.store(0)
  sharedState.trialBarrier.wait()
  // Race happens.
  sharedState.trialBarrier.wait()

  // Collect and compare results.
  for i in 0..<racingThreadCount {
    let shuffle = sharedState.workerStates[i].raceDataShuffle
    sharedState.workerStates[i].raceData =
      gather(sharedState.workerStates[i].raceData, shuffle)
    sharedState.workerStates[i].observations =
      gather(sharedState.workerStates[i].observations, shuffle)
  }
  if true {
    // FIXME: why doesn't the bracket syntax work?
    // <rdar://problem/18305718> Array sugar syntax does not work when used
    // with associated types
    var observations = _UnitTestArray<RT.Observation>()
    observations.reserveCapacity(racingThreadCount)
    for i in 0..<raceDataCount {
      for j in 0..<racingThreadCount {
        observations.append(sharedState.workerStates[j].observations[i])
      }
      var sink = SinkOf<RaceTestObservationEvaluation>({
        sharedState.aggregatedEvaluations.addEvaluation($0)
      })
      rt.evaluateObservations(observations, &sink)
      observations.removeAll(keepCapacity: true)
    }
  }
}

func _workerThreadOneTrial<RT : RaceTestWithPerTrialDataType>(
  tid: Int, sharedState: _RaceTestSharedState<RT>
) {
  sharedState.trialBarrier.wait()
  let racingThreadCount = sharedState.racingThreadCount
  let workerState = sharedState.workerStates[tid]
  let rt = RT()
  var threadLocalData = rt.makeThreadLocalData()
  if true {
    let trialSpinBarrier = sharedState.trialSpinBarrier
    trialSpinBarrier.fetchAndAdd(1)
    while trialSpinBarrier.load() < racingThreadCount {}
  }
  // Perform racy operations.
  // Warning: do not add any synchronization in this loop, including
  // any implicit reference counting of shared data.
  for raceData in workerState.raceData {
    workerState.observations.append(rt.thread1(raceData, &threadLocalData))
  }
  sharedState.trialBarrier.wait()
}

public func runRaceTest<RT : RaceTestWithPerTrialDataType>(
  _: RT.Type,
  #trials: Int,
  threads: Int? = nil
) {
  let racingThreadCount = threads ?? max(2, _stdlib_getHardwareConcurrency())
  let sharedState = _RaceTestSharedState<RT>(racingThreadCount: racingThreadCount)

  let masterThreadBody: (_: ())->() = {
    (_: ())->() in
    for trial in 0..<trials {
      autoreleasepool {
        _masterThreadOneTrial(sharedState)
      }
    }
  }

  let racingThreadBody: (Int)->() = {
    (tid: Int)->() in
    for trial in 0..<trials {
      _workerThreadOneTrial(tid, sharedState)
    }
  }

  var allTids = [pthread_t]()

  // Create the master thread.
  if true {
    let (ret, tid) = _stdlib_pthread_create_block(
      nil, masterThreadBody, ())
    expectEqual(0, ret)
    allTids.append(tid!)
  }

  // Create racing threads.
  for i in 0..<racingThreadCount {
    let (ret, tid) = _stdlib_pthread_create_block(
      nil, racingThreadBody, i)
    expectEqual(0, ret)
    allTids.append(tid!)
  }

  // Join all threads.
  for tid in allTids {
    let (ret, _) = _stdlib_pthread_join(tid, Void.self)
    expectEqual(0, ret)
  }

  let aggregatedEvaluations = sharedState.aggregatedEvaluations
  expectFalse(aggregatedEvaluations.isFailed)
  println(aggregatedEvaluations)
}

internal func _divideRoundUp(lhs: Int, rhs: Int) -> Int {
  return (lhs + rhs) / rhs
}

public func runRaceTest<RT : RaceTestWithPerTrialDataType>(
  test: RT.Type,
  #operations: Int,
  threads: Int? = nil
) {
  let racingThreadCount = threads ?? max(2, _stdlib_getHardwareConcurrency())

  // Each trial runs threads^2 operations.
  let operationsPerTrial = racingThreadCount * racingThreadCount
  let trials = _divideRoundUp(operations, operationsPerTrial)
  runRaceTest(test, trials: trials, threads: threads)
}

public func consumeCPU(#units: Int) {
  for i in 0..<units {
    let scale = 16
    for j in 0..<scale {
      _blackHole(42)
    }
  }
}
