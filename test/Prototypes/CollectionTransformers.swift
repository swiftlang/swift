//===----------------------------------------------------------------------===//
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
// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

public enum ApproximateCount {
  case Unknown
  case Precise(IntMax)
  case Underestimate(IntMax)
  case Overestimate(IntMax)
}

public protocol ApproximateCountableSequenceType : SequenceType {
  /// Complexity: amortized O(1).
  var approximateCount: ApproximateCount { get }
}

/// A collection that provides an efficient way to split its index ranges.
public protocol SplittableCollectionType : CollectionType {
  // We need this protocol so that collections with only forward or bidirectional
  // traversals could customize their splitting behavior.
  //
  // FIXME: all collections with random access should conform to this protocol
  // automatically.

  /// Splits a given range of indices into a set of disjoint ranges covering
  /// the same elements.
  ///
  /// Complexity: amortized O(1).
  ///
  /// FIXME: should that be O(log n) to cover some strange collections?
  ///
  /// FIXME: index invalidation rules?
  ///
  /// FIXME: a better name.  Users will never want to call this method
  /// directly.
  ///
  /// FIXME: return an optional for the common case when split() can not
  /// subdivide the range further.
  func split(range: Range<Index>) -> [Range<Index>]
}

internal func _splitRandomAccessIndexRange<Index : RandomAccessIndexType>(
  range: Range<Index>
) -> [Range<Index>] {
  let startIndex = range.startIndex
  let endIndex = range.endIndex
  let length = startIndex.distanceTo(endIndex).toIntMax()
  if length < 2 {
    return [ range ]
  }
  let middle = startIndex.advancedBy(Index.Distance(length / 2))
  return [
    Range(start: startIndex, end: middle),
    Range(start: middle, end: endIndex)
  ]
}

/// A helper object to build a collection incrementally in an efficient way.
///
/// Using a builder can be more efficient than creating an empty collection
/// instance and adding elements one by one.
public protocol CollectionBuilderType {
  typealias Collection : CollectionType
  typealias Element = Collection.Generator.Element

  init()

  /// Gives a hint about the expected approximate number of elements in the
  /// collection that is being built.
  mutating func sizeHint(approximateSize: Int)

  /// Append `element` to `self`.
  ///
  /// If a collection being built supports a user-defined order, the element is
  /// added at the end.
  ///
  /// Complexity: amortized O(1).
  mutating func append(element: Collection.Generator.Element)

  /// Append `elements` to `self`.
  ///
  /// If a collection being built supports a user-defined order, the element is
  /// added at the end.
  ///
  /// Complexity: amortized O(n), where `n` is equal to `count(elements)`.
  mutating func appendContentsOf<
    C : CollectionType
    where
    C.Generator.Element == Element
  >(elements: C)

  /// Append elements from `otherBuilder` to `self`, emptying `otherBuilder`.
  ///
  /// Equivalent to::
  ///
  ///   self.appendContentsOf(otherBuilder.takeResult())
  ///
  /// but is more efficient.
  ///
  /// Complexity: O(1).
  mutating func moveContentsOf(inout otherBuilder: Self)

  /// Build the collection from the elements that were added to this builder.
  ///
  /// Once this function is called, the builder may not be reused and no other
  /// methods should be called.
  ///
  /// Complexity: O(n) or better (where `n` is the number of elements that were
  /// added to this builder); typically O(1).
  mutating func takeResult() -> Collection
}

public protocol BuildableCollectionType : CollectionType {
  typealias Builder : CollectionBuilderType
}

extension Array : SplittableCollectionType {
  public func split(range: Range<Int>) -> [Range<Int>] {
    return _splitRandomAccessIndexRange(range)
  }
}

public struct ArrayBuilder<T> : CollectionBuilderType {
  // FIXME: the compiler didn't complain when I remove public on 'Collection'.
  // File a bug.
  public typealias Collection = Array<T>
  public typealias Element = T

  internal var _resultParts = [[T]]()
  internal var _resultTail = [T]()

  public init() {}

  public mutating func sizeHint(approximateSize: Int) {
    _resultTail.reserveCapacity(approximateSize)
  }

  public mutating func append(element: T) {
    _resultTail.append(element)
  }

  public mutating func appendContentsOf<
    C : CollectionType
    where
    C.Generator.Element == T
  >(elements: C) {
    _resultTail.appendContentsOf(elements)
  }

  public mutating func moveContentsOf(inout otherBuilder: ArrayBuilder<T>) {
    // FIXME: do something smart with the capacity set in this builder and the
    // other builder.
    _resultParts.append(_resultTail)
    _resultTail = []
    // FIXME: not O(1)!
    _resultParts.appendContentsOf(otherBuilder._resultParts)
    otherBuilder._resultParts = []
    swap(&_resultTail, &otherBuilder._resultTail)
  }

  public mutating func takeResult() -> Collection {
    _resultParts.append(_resultTail)
    _resultTail = []
    // FIXME: optimize.  parallelize.
    return Array(_resultParts.flatten())
  }
}

extension Array : BuildableCollectionType {
  public typealias Builder = ArrayBuilder<Element>
}

//===----------------------------------------------------------------------===//
// Fork-join
//===----------------------------------------------------------------------===//

// As sad as it is, I think for practical performance reasons we should rewrite
// the inner parts of the fork-join framework in C++.  In way too many cases
// than necessary Swift requires an extra allocation to pin objects in memory
// for safe multithreaded access.  -Dmitri

import SwiftShims
import SwiftPrivate
import Darwin
import Dispatch

// FIXME: port to Linux.
// XFAIL: linux

// A wrapper for pthread_t with platform-independent interface.
public struct _stdlib_pthread_t : Equatable, Hashable {
  internal let _value: pthread_t

  public var hashValue: Int {
    return _value.hashValue
  }
}

public func == (lhs: _stdlib_pthread_t, rhs: _stdlib_pthread_t) -> Bool {
  return lhs._value == rhs._value
}

public func _stdlib_pthread_self() -> _stdlib_pthread_t {
  return _stdlib_pthread_t(_value: pthread_self())
}

struct _ForkJoinMutex {
  var _mutex: UnsafeMutablePointer<pthread_mutex_t>

  init() {
    _mutex = UnsafeMutablePointer.alloc(1)
    if pthread_mutex_init(_mutex, nil) != 0 {
      fatalError("pthread_mutex_init")
    }
  }

  func `deinit`() {
    if pthread_mutex_destroy(_mutex) != 0 {
      fatalError("pthread_mutex_init")
    }
    _mutex.destroy()
    _mutex.dealloc(1)
  }

  func withLock<Result>(@noescape body: () -> Result) -> Result {
    if pthread_mutex_lock(_mutex) != 0 {
      fatalError("pthread_mutex_lock")
    }
    let result = body()
    if pthread_mutex_unlock(_mutex) != 0 {
      fatalError("pthread_mutex_unlock")
    }
    return result
  }
}

struct _ForkJoinCond {
  var _cond: UnsafeMutablePointer<pthread_cond_t> = nil

  init() {
    _cond = UnsafeMutablePointer.alloc(1)
    if pthread_cond_init(_cond, nil) != 0 {
      fatalError("pthread_cond_init")
    }
  }

  func `deinit`() {
    if pthread_cond_destroy(_cond) != 0 {
      fatalError("pthread_cond_destroy")
    }
    _cond.destroy()
    _cond.dealloc(1)
  }

  func signal() {
    pthread_cond_signal(_cond)
  }

  func wait(mutex: _ForkJoinMutex) {
    pthread_cond_wait(_cond, mutex._mutex)
  }
}

final class _ForkJoinOneShotEvent {
  var _mutex: _ForkJoinMutex = _ForkJoinMutex()
  var _cond: _ForkJoinCond = _ForkJoinCond()
  var _isSet: Bool = false

  init() {}

  deinit {
    _cond.`deinit`()
    _mutex.`deinit`()
  }

  func set() {
    _mutex.withLock {
      if !_isSet {
        _isSet = true
        _cond.signal()
      }
    }
  }

  /// Establishes a happens-before relation between calls to set() and wait().
  func wait() {
    _mutex.withLock {
      while !_isSet {
        _cond.wait(_mutex)
      }
    }
  }

  /// If the function returns true, it establishes a happens-before relation
  /// between calls to set() and isSet().
  func isSet() -> Bool {
    return _mutex.withLock {
      return _isSet
    }
  }
}

final class _ForkJoinWorkDeque<T> {
  // FIXME: this is just a proof-of-concept; very inefficient.

  // Implementation note: adding elements to the head of the deque is common in
  // fork-join, so _deque is stored reversed (appending to an array is cheap).
  // FIXME: ^ that is false for submission queues though.
  var _deque: ContiguousArray<T> = []
  var _dequeMutex: _ForkJoinMutex = _ForkJoinMutex()

  init() {}

  deinit {
    precondition(_deque.isEmpty)

    _dequeMutex.`deinit`()
  }

  var isEmpty: Bool {
    return _dequeMutex.withLock {
      return _deque.isEmpty
    }
  }

  func prepend(element: T) {
    _dequeMutex.withLock {
      _deque.append(element)
    }
  }

  func tryTakeFirst() -> T? {
    return _dequeMutex.withLock {
      let result = _deque.last
      if _deque.count > 0 {
        _deque.removeLast()
      }
      return result
    }
  }

  func tryTakeFirstTwo() -> (T?, T?) {
    return _dequeMutex.withLock {
      let result1 = _deque.last
      if _deque.count > 0 {
        _deque.removeLast()
      }
      let result2 = _deque.last
      if _deque.count > 0 {
        _deque.removeLast()
      }
      return (result1, result2)
    }
  }

  func append(element: T) {
    _dequeMutex.withLock {
      _deque.insert(element, atIndex: 0)
    }
  }

  func tryTakeLast() -> T? {
    return _dequeMutex.withLock {
      let result = _deque.first
      if _deque.count > 0 {
        _deque.removeAtIndex(0)
      }
      return result
    }
  }

  func takeAll() -> ContiguousArray<T> {
    return _dequeMutex.withLock {
      let result = _deque
      _deque = []
      return result
    }
  }

  func tryReplace(
    value: T,
    makeReplacement: () -> T,
    isEquivalent: (T, T) -> Bool
  ) -> Bool {
    return _dequeMutex.withLock {
      for i in _deque.indices {
        if isEquivalent(_deque[i], value) {
          _deque[i] = makeReplacement()
          return true
        }
      }
      return false
    }
  }
}

final class _ForkJoinWorkerThread {
  internal var _tid: _stdlib_pthread_t?
  internal let _pool: ForkJoinPool
  internal let _submissionQueue: _ForkJoinWorkDeque<ForkJoinTaskBase>
  internal let _workDeque: _ForkJoinWorkDeque<ForkJoinTaskBase>

  internal init(
    _pool: ForkJoinPool,
    submissionQueue: _ForkJoinWorkDeque<ForkJoinTaskBase>,
    workDeque: _ForkJoinWorkDeque<ForkJoinTaskBase>
  ) {
    self._tid = nil
    self._pool = _pool
    self._submissionQueue = submissionQueue
    self._workDeque = workDeque
  }

  internal func startAsync() {
    var queue: dispatch_queue_t? = nil
    if #available(OSX 10.10, iOS 8.0, *) {
      queue = dispatch_get_global_queue(QOS_CLASS_BACKGROUND, 0)
    } else {
      queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_BACKGROUND, 0)
    }
    dispatch_async(queue!) {
      self._thread()
    }
  }

  internal func _thread() {
    print("_ForkJoinWorkerThread begin")
    _tid = _stdlib_pthread_self()
    outer: while !_workDeque.isEmpty || !_submissionQueue.isEmpty {
      _pool._addRunningThread(self)
      while true {
        if _pool._tryStopThread() {
          print("_ForkJoinWorkerThread detected too many threads")
          _pool._removeRunningThread(self)
          _pool._submitTasksToRandomWorkers(_workDeque.takeAll())
          _pool._submitTasksToRandomWorkers(_submissionQueue.takeAll())
          print("_ForkJoinWorkerThread end")
          return
        }

        // Process tasks in FIFO order: first the work queue, then the
        // submission queue.
        if let task = _workDeque.tryTakeFirst() {
          task._run()
          continue
        }
        if let task = _submissionQueue.tryTakeFirst() {
          task._run()
          continue
        }

        print("_ForkJoinWorkerThread stealing tasks")
        if let task = _pool._stealTask() {
          task._run()
          continue
        }

        // FIXME: steal from submission queues?

        break
      }
      _pool._removeRunningThread(self)
    }
    assert(_workDeque.isEmpty)
    assert(_submissionQueue.isEmpty)
    _pool._totalThreads.fetchAndAdd(-1)
    print("_ForkJoinWorkerThread end")
  }

  internal func _forkTask(task: ForkJoinTaskBase) {
    // Try to inflate the pool.
    if !_pool._tryCreateThread({ task }) {
      _workDeque.prepend(task)
    }
  }

  internal func _waitForTask(task: ForkJoinTaskBase) {
    while true {
      if task._isComplete() {
        return
      }

      // If the task is in work queue of the current thread, run the task.
      if _workDeque.tryReplace(
        task,
        makeReplacement: { ForkJoinTask<()>() {} },
        isEquivalent: { $0 === $1 }) {

        // We found the task.  Run it in-place.
        task._run()
        return
      }

      // FIXME: also check the submission queue, maybe the task is there?

      // FIXME: try to find the task in other threads' queues.

      // FIXME: try to find tasks that were forked from this task in other
      // threads' queues.  Help thieves by stealing those tasks back.

      // At this point, we can't do any work to help with running this task.
      // We can't start new work either (if we do, we might end up creating
      // more in-flight work than we can chew, and crash with out-of-memory
      // errors).
      _pool._compensateForBlockedWorkerThread() {
        task._blockingWait()
        // FIXME: do a timed wait, and retry stealing.
      }
    }
  }
}

internal protocol _FutureType {
  typealias Result

  /// Establishes a happens-before relation between completing the future and
  /// the call to wait().
  func wait()

  func tryGetResult() -> Result?
  func tryTakeResult() -> Result?

  func waitAndGetResult() -> Result
  func waitAndTakeResult() -> Result
}

public class ForkJoinTaskBase {
  final internal var _pool: ForkJoinPool? = nil

  // FIXME(performance): there is no need to create heavy-weight
  // synchronization primitives every time.  We could start with a lightweight
  // atomic int for the flag and inflate to a full event when needed.  Unless
  // we really need to block in wait(), we would avoid creating an event.
  final internal let _completedEvent: _ForkJoinOneShotEvent =
    _ForkJoinOneShotEvent()

  final internal func _isComplete() -> Bool {
    return _completedEvent.isSet()
  }

  final internal func _blockingWait() {
    _completedEvent.wait()
  }

  internal func _run() {
    fatalError("implement")
  }

  final public func fork() {
    precondition(_pool == nil)
    if let thread = ForkJoinPool._getCurrentThread() {
      thread._forkTask(self)
    } else {
      // FIXME: decide if we want to allow this.
      precondition(false)
      ForkJoinPool.commonPool.forkTask(self)
    }
  }

  final public func wait() {
    if let thread = ForkJoinPool._getCurrentThread() {
      thread._waitForTask(self)
    } else {
      _blockingWait()
    }
  }
}

final public class ForkJoinTask<Result> : ForkJoinTaskBase, _FutureType {
  internal let _task: () -> Result
  internal var _result: Result? = nil

  public init(_task: () -> Result) {
    self._task = _task
  }

  override internal func _run() {
    _complete(_task())
  }

  /// It is not allowed to call _complete() in a racy way.  Only one thread
  /// should ever call _complete().
  internal func _complete(result: Result) {
    precondition(!_completedEvent.isSet())
    _result = result
    _completedEvent.set()
  }

  public func tryGetResult() -> Result? {
    if _completedEvent.isSet() {
      return _result
    }
    return nil
  }

  public func tryTakeResult() -> Result? {
    if _completedEvent.isSet() {
      let result = _result
      _result = nil
      return result
    }
    return nil
  }

  public func waitAndGetResult() -> Result {
    wait()
    return tryGetResult()!
  }

  public func waitAndTakeResult() -> Result {
    wait()
    return tryTakeResult()!
  }
}

final public class ForkJoinPool {
  internal static var _threadRegistry: [_stdlib_pthread_t : _ForkJoinWorkerThread] = [:]
  internal static var _threadRegistryMutex: _ForkJoinMutex = _ForkJoinMutex()

  internal static func _getCurrentThread() -> _ForkJoinWorkerThread? {
    return _threadRegistryMutex.withLock {
      return _threadRegistry[_stdlib_pthread_self()]
    }
  }

  internal let _maxThreads: Int
  /// Total number of threads: number of running threads plus the number of
  /// threads that are preparing to start).
  internal let _totalThreads: _stdlib_AtomicInt = _stdlib_AtomicInt(0)

  internal var _runningThreads: [_ForkJoinWorkerThread] = []
  internal var _runningThreadsMutex: _ForkJoinMutex = _ForkJoinMutex()

  internal var _submissionQueues: [_ForkJoinWorkDeque<ForkJoinTaskBase>] = []
  internal var _submissionQueuesMutex: _ForkJoinMutex = _ForkJoinMutex()

  internal var _workDeques: [_ForkJoinWorkDeque<ForkJoinTaskBase>] = []
  internal var _workDequesMutex: _ForkJoinMutex = _ForkJoinMutex()

  internal init(_commonPool: ()) {
    self._maxThreads = _stdlib_getHardwareConcurrency()
  }

  deinit {
    _runningThreadsMutex.`deinit`()
    _submissionQueuesMutex.`deinit`()
    _workDequesMutex.`deinit`()
  }

  internal func _addRunningThread(thread: _ForkJoinWorkerThread) {
    ForkJoinPool._threadRegistryMutex.withLock {
      _runningThreadsMutex.withLock {
        _submissionQueuesMutex.withLock {
          _workDequesMutex.withLock {
            ForkJoinPool._threadRegistry[thread._tid!] = thread
            _runningThreads.append(thread)
            _submissionQueues.append(thread._submissionQueue)
            _workDeques.append(thread._workDeque)
          }
        }
      }
    }
  }

  internal func _removeRunningThread(thread: _ForkJoinWorkerThread) {
    ForkJoinPool._threadRegistryMutex.withLock {
      _runningThreadsMutex.withLock {
        _submissionQueuesMutex.withLock {
          _workDequesMutex.withLock {
            let i = _runningThreads.indexOf { $0 === thread }!
            ForkJoinPool._threadRegistry[thread._tid!] = nil
            _runningThreads.removeAtIndex(i)
            _submissionQueues.removeAtIndex(i)
            _workDeques.removeAtIndex(i)
          }
        }
      }
    }
  }

  internal func _compensateForBlockedWorkerThread(blockingBody: () -> ()) {
    // FIXME: limit the number of compensating threads.
    let submissionQueue = _ForkJoinWorkDeque<ForkJoinTaskBase>()
    let workDeque = _ForkJoinWorkDeque<ForkJoinTaskBase>()
    let thread = _ForkJoinWorkerThread(
      _pool: self, submissionQueue: submissionQueue, workDeque: workDeque)
    thread.startAsync()
    blockingBody()
    _totalThreads.fetchAndAdd(1)
  }

  internal func _tryCreateThread(
    @noescape makeTask: () -> ForkJoinTaskBase?
  ) -> Bool {
    var success = false
    var oldNumThreads = _totalThreads.load()
    repeat {
      if oldNumThreads >= _maxThreads {
        return false
      }
      success = _totalThreads.compareExchange(
        expected: &oldNumThreads, desired: oldNumThreads + 1)
    } while !success
    if let task = makeTask() {
      let submissionQueue = _ForkJoinWorkDeque<ForkJoinTaskBase>()
      let workDeque = _ForkJoinWorkDeque<ForkJoinTaskBase>()
      workDeque.prepend(task)
      let thread = _ForkJoinWorkerThread(
        _pool: self, submissionQueue: submissionQueue, workDeque: workDeque)
      thread.startAsync()
    } else {
      _totalThreads.fetchAndAdd(-1)
    }
    return true
  }

  internal func _stealTask() -> ForkJoinTaskBase? {
    return _workDequesMutex.withLock {
      let randomOffset = pickRandom(_workDeques.indices)
      let count = _workDeques.count
      for i in _workDeques.indices {
        let index = (i + randomOffset) % count
        if let task = _workDeques[index].tryTakeLast() {
          return task
        }
      }
      return nil
    }
  }

  /// Check if the pool has grown too large because of compensating
  /// threads.
  internal func _tryStopThread() -> Bool {
    var success = false
    var oldNumThreads = _totalThreads.load()
    repeat {
      // FIXME: magic number 2.
      if oldNumThreads <= _maxThreads + 2 {
        return false
      }
      success = _totalThreads.compareExchange(
        expected: &oldNumThreads, desired: oldNumThreads - 1)
    } while !success
    return true
  }

  internal func _submitTasksToRandomWorkers<
    C : CollectionType
    where
    C.Generator.Element == ForkJoinTaskBase
  >(tasks: C) {
    if tasks.isEmpty {
      return
    }
    _submissionQueuesMutex.withLock {
      precondition(!_submissionQueues.isEmpty)
      for task in tasks {
        pickRandom(_submissionQueues).append(task)
      }
    }
  }

  public func forkTask(task: ForkJoinTaskBase) {
    while true {
      // Try to inflate the pool first.
      if _tryCreateThread({ task }) {
        return
      }

      // Looks like we can't create more threads.  Submit the task to
      // a random thread.
      let done = _submissionQueuesMutex.withLock {
        () -> Bool in
        if !_submissionQueues.isEmpty {
          pickRandom(_submissionQueues).append(task)
          return true
        }
        return false
      }
      if done {
        return
      }
    }
  }

  // FIXME: return a Future instead?
  public func forkTask<Result>(task: () -> Result) -> ForkJoinTask<Result> {
    let forkJoinTask = ForkJoinTask(_task: task)
    forkTask(forkJoinTask)
    return forkJoinTask
  }

  public static var commonPool = ForkJoinPool(_commonPool: ())

  public static func invokeAll(tasks: ForkJoinTaskBase...) {
    ForkJoinPool.invokeAll(tasks)
  }

  public static func invokeAll(tasks: [ForkJoinTaskBase]) {
    if tasks.isEmpty {
      return
    }
    if ForkJoinPool._getCurrentThread() != nil {
      // Run the first task in this thread, fork the rest.
      let first = tasks.first
      for t in tasks.dropFirst() {
        // FIXME: optimize forking in bulk.
        t.fork()
      }
      first!._run()
    } else {
      // FIXME: decide if we want to allow this.
      precondition(false)
    }
  }
}

//===----------------------------------------------------------------------===//
// Collection transformation DSL: implementation
//===----------------------------------------------------------------------===//

internal protocol _CollectionTransformerStepType /*: class*/ {
  typealias PipelineInputElement
  typealias OutputElement

  func transform<
    InputCollection : CollectionType,
    Collector : _ElementCollectorType
    where
    InputCollection.Generator.Element == PipelineInputElement,
    Collector.Element == OutputElement
  >(
    c: InputCollection,
    _ range: Range<InputCollection.Index>,
    inout _ collector: Collector
  )
}

internal class _CollectionTransformerStep<PipelineInputElement_, OutputElement_>
  : _CollectionTransformerStepType {

  typealias PipelineInputElement = PipelineInputElement_
  typealias OutputElement = OutputElement_

  func map<U>(transform: (OutputElement) -> U)
    -> _CollectionTransformerStep<PipelineInputElement, U> {

    fatalError("abstract method")
  }

  func filter(predicate: (OutputElement) -> Bool)
    -> _CollectionTransformerStep<PipelineInputElement, OutputElement> {

    fatalError("abstract method")
  }

  func reduce<U>(initial: U, _ combine: (U, OutputElement) -> U)
    -> _CollectionTransformerFinalizer<PipelineInputElement, U> {

    fatalError("abstract method")
  }

  func collectTo<
    C : BuildableCollectionType
    where
    C.Builder.Collection == C,
    C.Builder.Element == C.Generator.Element,
    C.Generator.Element == OutputElement
  >(_: C.Type) -> _CollectionTransformerFinalizer<PipelineInputElement, C> {

    fatalError("abstract method")
  }

  func transform<
    InputCollection : CollectionType,
    Collector : _ElementCollectorType
    where
    InputCollection.Generator.Element == PipelineInputElement,
    Collector.Element == OutputElement
  >(
    c: InputCollection,
    _ range: Range<InputCollection.Index>,
    inout _ collector: Collector
  ) {
    fatalError("abstract method")
  }
}

final internal class _CollectionTransformerStepCollectionSource<
  PipelineInputElement
> : _CollectionTransformerStep<PipelineInputElement, PipelineInputElement> {

  typealias InputElement = PipelineInputElement

  override func map<U>(transform: (InputElement) -> U)
    -> _CollectionTransformerStep<PipelineInputElement, U> {

    return _CollectionTransformerStepOneToMaybeOne(self) {
      transform($0)
    }
  }

  override func filter(predicate: (InputElement) -> Bool)
    -> _CollectionTransformerStep<PipelineInputElement, InputElement> {

    return _CollectionTransformerStepOneToMaybeOne(self) {
      predicate($0) ? $0 : nil
    }
  }

  override func reduce<U>(initial: U, _ combine: (U, InputElement) -> U)
    -> _CollectionTransformerFinalizer<PipelineInputElement, U> {

    return _CollectionTransformerFinalizerReduce(self, initial, combine)
  }

  override func collectTo<
    C : BuildableCollectionType
    where
    C.Builder.Collection == C,
    C.Builder.Element == C.Generator.Element,
    C.Generator.Element == OutputElement
  >(c: C.Type) -> _CollectionTransformerFinalizer<PipelineInputElement, C> {

    return _CollectionTransformerFinalizerCollectTo(self, c)
  }

  override func transform<
    InputCollection : CollectionType,
    Collector : _ElementCollectorType
    where
    InputCollection.Generator.Element == PipelineInputElement,
    Collector.Element == OutputElement
  >(
    c: InputCollection,
    _ range: Range<InputCollection.Index>,
    inout _ collector: Collector
  ) {
    for i in range {
      let e = c[i]
      collector.append(e)
    }
  }
}

final internal class _CollectionTransformerStepOneToMaybeOne<
  PipelineInputElement,
  OutputElement,
  InputStep : _CollectionTransformerStepType
  where
  InputStep.PipelineInputElement == PipelineInputElement
> : _CollectionTransformerStep<PipelineInputElement, OutputElement> {

  typealias _Self = _CollectionTransformerStepOneToMaybeOne
  typealias InputElement = InputStep.OutputElement

  let _input: InputStep
  let _transform: (InputElement) -> OutputElement?

  init(_ input: InputStep, _ transform: (InputElement) -> OutputElement?) {
    self._input = input
    self._transform = transform
    super.init()
  }

  override func map<U>(transform: (OutputElement) -> U)
    -> _CollectionTransformerStep<PipelineInputElement, U> {

    // Let the closure below capture only one variable, not the whole `self`.
    let localTransform = _transform
    return _CollectionTransformerStepOneToMaybeOne<PipelineInputElement, U, InputStep>(_input) {
      (input: InputElement) -> U? in
      if let e = localTransform(input) {
        return transform(e)
      }
      return nil
    }
  }

  override func filter(predicate: (OutputElement) -> Bool)
    -> _CollectionTransformerStep<PipelineInputElement, OutputElement> {

    // Let the closure below capture only one variable, not the whole `self`.
    let localTransform = _transform
    return _CollectionTransformerStepOneToMaybeOne<PipelineInputElement, OutputElement, InputStep>(_input) {
      (input: InputElement) -> OutputElement? in
      if let e = localTransform(input) {
        return predicate(e) ? e : nil
      }
      return nil
    }
  }

  override func reduce<U>(initial: U, _ combine: (U, OutputElement) -> U)
    -> _CollectionTransformerFinalizer<PipelineInputElement, U> {

    return _CollectionTransformerFinalizerReduce(self, initial, combine)
  }

  override func collectTo<
    C : BuildableCollectionType
    where
    C.Builder.Collection == C,
    C.Builder.Element == C.Generator.Element,
    C.Generator.Element == OutputElement
  >(c: C.Type) -> _CollectionTransformerFinalizer<PipelineInputElement, C> {

    return _CollectionTransformerFinalizerCollectTo(self, c)
  }

  override func transform<
    InputCollection : CollectionType,
    Collector : _ElementCollectorType
    where
    InputCollection.Generator.Element == PipelineInputElement,
    Collector.Element == OutputElement
  >(
    c: InputCollection,
    _ range: Range<InputCollection.Index>,
    inout _ collector: Collector
  ) {
    var collectorWrapper =
      _ElementCollectorOneToMaybeOne(collector, _transform)
    _input.transform(c, range, &collectorWrapper)
    collector = collectorWrapper._baseCollector
  }
}

struct _ElementCollectorOneToMaybeOne<
  BaseCollector : _ElementCollectorType,
  Element_
> : _ElementCollectorType {
  typealias Element = Element_

  var _baseCollector: BaseCollector
  var _transform: (Element) -> BaseCollector.Element?

  init(
    _ baseCollector: BaseCollector,
    _ transform: (Element) -> BaseCollector.Element?
  ) {
    self._baseCollector = baseCollector
    self._transform = transform
  }

  mutating func sizeHint(approximateSize: Int) {}

  mutating func append(element: Element) {
    if let e = _transform(element) {
      _baseCollector.append(e)
    }
  }

  mutating func appendContentsOf<
    C : CollectionType
    where
    C.Generator.Element == Element
  >(elements: C) {
    for e in elements {
      append(e)
    }
  }
}

protocol _ElementCollectorType {
  typealias Element

  mutating func sizeHint(approximateSize: Int)

  mutating func append(element: Element)

  mutating func appendContentsOf<
    C : CollectionType
    where
    C.Generator.Element == Element
  >(elements: C)
}

class _CollectionTransformerFinalizer<PipelineInputElement, Result> {
  func transform<
    InputCollection : CollectionType
    where
    InputCollection.Generator.Element == PipelineInputElement
  >(c: InputCollection) -> Result {
    fatalError("implement")
  }
}

final class _CollectionTransformerFinalizerReduce<
  PipelineInputElement,
  U,
  InputElementTy,
  InputStep : _CollectionTransformerStepType
  where
  InputStep.OutputElement == InputElementTy,
  InputStep.PipelineInputElement == PipelineInputElement
> : _CollectionTransformerFinalizer<PipelineInputElement, U> {

  var _input: InputStep
  var _initial: U
  var _combine: (U, InputElementTy) -> U

  init(_ input: InputStep, _ initial: U, _ combine: (U, InputElementTy) -> U) {
    self._input = input
    self._initial = initial
    self._combine = combine
  }

  override func transform<
    InputCollection : CollectionType
    where
    InputCollection.Generator.Element == PipelineInputElement
  >(c: InputCollection) -> U {
    var collector = _ElementCollectorReduce(_initial, _combine)
    _input.transform(c, c.indices, &collector)
    return collector.takeResult()
  }
}

struct _ElementCollectorReduce<Element_, Result> : _ElementCollectorType {
  typealias Element = Element_

  var _current: Result
  var _combine: (Result, Element) -> Result

  init(_ initial: Result, _ combine: (Result, Element) -> Result) {
    self._current = initial
    self._combine = combine
  }

  mutating func sizeHint(approximateSize: Int) {}

  mutating func append(element: Element) {
    _current = _combine(_current, element)
  }

  mutating func appendContentsOf<
    C : CollectionType
    where
    C.Generator.Element == Element
  >(elements: C) {
    for e in elements {
      append(e)
    }
  }

  mutating func takeResult() -> Result {
    return _current
  }
}

final class _CollectionTransformerFinalizerCollectTo<
  PipelineInputElement,
  U : BuildableCollectionType,
  InputElementTy,
  InputStep : _CollectionTransformerStepType
  where
  InputStep.OutputElement == InputElementTy,
  InputStep.PipelineInputElement == PipelineInputElement,
  U.Builder.Collection == U,
  U.Builder.Element == U.Generator.Element,
  U.Generator.Element == InputStep.OutputElement
> : _CollectionTransformerFinalizer<PipelineInputElement, U> {

  var _input: InputStep

  init(_ input: InputStep, _: U.Type) {
    self._input = input
  }

  override func transform<
    InputCollection : CollectionType
    where
    InputCollection.Generator.Element == PipelineInputElement
  >(c: InputCollection) -> U {
    var collector = _ElementCollectorCollectTo<U>()
    _input.transform(c, c.indices, &collector)
    return collector.takeResult()
  }
}

struct _ElementCollectorCollectTo<
  Collection : BuildableCollectionType
  where
  Collection.Builder.Collection == Collection,
  Collection.Builder.Element == Collection.Generator.Element
> : _ElementCollectorType {

  typealias Element = Collection.Generator.Element

  var _builder: Collection.Builder

  init() {
    self._builder = Collection.Builder()
  }

  mutating func sizeHint(approximateSize: Int) {
    _builder.sizeHint(approximateSize)
  }

  mutating func append(element: Element) {
    _builder.append(element)
  }

  mutating func appendContentsOf<
    C : CollectionType
    where
    C.Generator.Element == Element
  >(elements: C) {
    _builder.appendContentsOf(elements)
  }

  mutating func takeResult() -> Collection {
    return _builder.takeResult()
  }
}

internal func _optimizeCollectionTransformer<PipelineInputElement, Result>(
  transformer: _CollectionTransformerFinalizer<PipelineInputElement, Result>
) -> _CollectionTransformerFinalizer<PipelineInputElement, Result> {
  return transformer
}

internal func _runCollectionTransformer<
  InputCollection : CollectionType, Result
>(
  c: InputCollection,
  _ transformer: _CollectionTransformerFinalizer<InputCollection.Generator.Element, Result>
) -> Result {
  dump(transformer)
  let optimized = _optimizeCollectionTransformer(transformer)
  dump(optimized)
  return transformer.transform(c)
}

//===----------------------------------------------------------------------===//
// Collection transformation DSL: public interface
//===----------------------------------------------------------------------===//

public struct CollectionTransformerPipeline<
  InputCollection : CollectionType, T
> {
  internal var _input: InputCollection
  internal var _step: _CollectionTransformerStep<InputCollection.Generator.Element, T>

  public func map<U>(transform: (T) -> U)
    -> CollectionTransformerPipeline<InputCollection, U> {

    return CollectionTransformerPipeline<InputCollection, U>(
      _input: _input,
      _step: _step.map(transform)
    )
  }

  public func filter(predicate: (T) -> Bool)
    -> CollectionTransformerPipeline<InputCollection, T> {

    return CollectionTransformerPipeline<InputCollection, T>(
      _input: _input,
      _step: _step.filter(predicate)
    )
  }

  public func reduce<U>(initial: U, _ combine: (U, T) -> U) -> U {
    return _runCollectionTransformer(_input, _step.reduce(initial, combine))
  }

  public func collectTo<
    C : BuildableCollectionType
    where
    C.Builder.Collection == C,
    C.Generator.Element == T,
    C.Builder.Element == T
  >(c: C.Type) -> C {
    return _runCollectionTransformer(_input, _step.collectTo(c))
  }

  public func toArray() -> [T] {
    return collectTo(Array<T>.self)
  }
}

public func transform<C : CollectionType>(c: C)
  -> CollectionTransformerPipeline<C, C.Generator.Element> {

  return CollectionTransformerPipeline<C, C.Generator.Element>(
    _input: c,
    _step: _CollectionTransformerStepCollectionSource<C.Generator.Element>())
}

//===----------------------------------------------------------------------===//
// Collection transformation DSL: tests
//===----------------------------------------------------------------------===//

import StdlibUnittest

var t = TestSuite("t")

t.test("fusion/map+reduce") {
  let xs = [ 1, 2, 3 ]
  let result =
    transform(xs)
    .map { $0 * 2 }
    .reduce(0, { $0 + $1 })
  expectEqual(12, result)
}

t.test("fusion/map+filter+reduce") {
  let xs = [ 1, 2, 3 ]
  let result = transform(xs)
    .map { $0 * 2 }
    .filter { $0 != 0 }
    .reduce(0, { $0 + $1 })
  expectEqual(12, result)
}

t.test("fusion/map+collectTo") {
  let xs = [ 1, 2, 3 ]
  let result =
    transform(xs)
    .map { $0 * 2 }
    .collectTo(Array<Int>.self)
  expectEqual([ 2, 4, 6 ], result)
}

t.test("fusion/map+toArray") {
  let xs = [ 1, 2, 3 ]
  let result =
    transform(xs)
    .map { $0 * 2 }
    .toArray()
  expectEqual([ 2, 4, 6 ], result)
}

t.test("ForkJoinPool.forkTask") {
  var tasks: [ForkJoinTask<()>] = []
  for i in 0..<100 {
    tasks.append(ForkJoinPool.commonPool.forkTask {
      () -> () in
      var result = 1
      for i in 0..<10000 {
        result = result &* i
        _blackHole(result)
      }
      return ()
    })
  }
  for t in tasks {
    t.wait()
  }
}

func fib(n: Int) -> Int {
  if n == 1 || n == 2 {
    return 1
  }
  if n == 38 {
    print("\(pthread_self()) fib(\(n))")
  }
  if n < 39 {
    let r = fib(n - 1) + fib(n - 2)
    _blackHole(r)
    return r
  }
  print("fib(\(n))")
  let t1 = ForkJoinTask() { fib(n - 1) }
  let t2 = ForkJoinTask() { fib(n - 2) }
  ForkJoinPool.invokeAll(t1, t2)
  return t2.waitAndGetResult() + t1.waitAndGetResult()
}

t.test("ForkJoinPool.forkTask/Fibonacci") {
  let t = ForkJoinPool.commonPool.forkTask { fib(40) }
  expectEqual(102334155, t.waitAndGetResult())
}

func _parallelMap(input: [Int], transform: (Int) -> Int, range: Range<Int>)
  -> Array<Int>.Builder {

  var builder = Array<Int>.Builder()
  if range.count < 1_000 {
    builder.appendContentsOf(input[range].map(transform))
  } else {
    let tasks = input.split(range).map {
      (subRange) in
      ForkJoinTask<Array<Int>.Builder> {
        _parallelMap(input, transform: transform, range: subRange)
      }
    }
    ForkJoinPool.invokeAll(tasks)
    for t in tasks {
      var otherBuilder = t.waitAndGetResult()
      builder.moveContentsOf(&otherBuilder)
    }
  }
  return builder
}

func parallelMap(input: [Int], transform: (Int) -> Int) -> [Int] {
  let t = ForkJoinPool.commonPool.forkTask {
    _parallelMap(input, transform: transform, range: input.indices)
  }
  var builder = t.waitAndGetResult()
  return builder.takeResult()
}

t.test("ForkJoinPool.forkTask/MapArray") {
  expectEqual(
    Array(2..<1_001),
    parallelMap(Array(1..<1_000)) { $0 + 1 }
  )
}

/*
 * FIXME: reduce compiler crasher
t.test("ForkJoinPool.forkTask") {
  func fib(n: Int) -> Int {
    if n == 0 || n == 1 {
      return 1
    }
    let t1 = ForkJoinPool.commonPool.forkTask { fib(n - 1) }
    let t2 = ForkJoinPool.commonPool.forkTask { fib(n - 2) }
    return t2.waitAndGetResult() + t1.waitAndGetResult()
  }
  expectEqual(0, fib(10))
}
*/


/*

Useful links:

http://habrahabr.ru/post/255659/

*/

runAllTests()
