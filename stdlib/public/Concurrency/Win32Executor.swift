//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if os(Windows)

import Swift
import WinSDK

internal import Synchronization

@available(SwiftStdlib 6.2, *)
extension ExecutorJob {

  fileprivate var win32ThreadPoolExecutor: UnownedTaskExecutor {
    get {
      return unsafe withUnsafeExecutorPrivateData {
        unsafe $0.withMemoryRebound(to: UnownedTaskExecutor.self) {
          return unsafe $0[0]
        }
      }
    }
    set {
      unsafe withUnsafeExecutorPrivateData {
        unsafe $0.withMemoryRebound(to: UnownedTaskExecutor.self) {
          unsafe $0[0] = newValue
        }
      }
    }
  }

  // The code below deals with the fact that the Timestamp may not fit
  // into the private storage.  If it doesn't, we need to allocate it
  // somewhere and store a pointer instead.

  fileprivate var win32TimestampIsIndirect: Bool {
    return unsafe MemoryLayout<OpaquePointer>.stride * 2
      < MemoryLayout<Win32EventLoopExecutor.Timestamp>.size
  }

  fileprivate var win32TimestampPointer: UnsafeMutablePointer<Win32EventLoopExecutor.Timestamp> {
    get {
      assert(win32TimestampIsIndirect)
      return unsafe withUnsafeExecutorPrivateData {
        unsafe $0.withMemoryRebound(to: UnsafeMutablePointer<Win32EventLoopExecutor.Timestamp>.self) {
          return unsafe $0[0]
        }
      }
    }
    set {
      assert(win32TimestampIsIndirect)
      unsafe withUnsafeExecutorPrivateData {
        unsafe $0.withMemoryRebound(to: UnsafeMutablePointer<Win32EventLoopExecutor.Timestamp>.self) {
          unsafe $0[0] = newValue
        }
      }
    }
  }

  fileprivate var win32Timestamp: Win32EventLoopExecutor.Timestamp {
    get {
      if win32TimestampIsIndirect {
        let ptr = unsafe win32TimestampPointer
        return unsafe ptr.pointee
      } else {
        return unsafe withUnsafeExecutorPrivateData {
          return unsafe $0.assumingMemoryBound(
            to: Win32EventLoopExecutor.Timestamp.self
          )[0]
        }
      }
    }
    set {
      if win32TimestampIsIndirect {
        let ptr = unsafe win32TimestampPointer
        unsafe ptr.pointee = newValue
     } else {
        unsafe withUnsafeExecutorPrivateData {
          unsafe $0.withMemoryRebound(to: Win32EventLoopExecutor.Timestamp.self) {
            unsafe $0[0] = newValue
          }
        }
      }
    }
  }

  fileprivate var win32Sequence: UInt {
    get {
      return unsafe withUnsafeExecutorPrivateData {
        return unsafe $0.assumingMemoryBound(to: UInt.self)[0]
      }
    }
    set {
      return unsafe withUnsafeExecutorPrivateData {
        unsafe $0.withMemoryRebound(to: UInt.self) {
          unsafe $0[0] = newValue
        }
      }
    }
  }

  fileprivate mutating func setupWin32Timestamp() {
    // If a Timestamp won't fit, allocate
    if win32TimestampIsIndirect {
      let ptr: UnsafeMutablePointer<Win32EventLoopExecutor.Timestamp>
      // Try to use the task allocator if it has one
      if let allocator {
        unsafe ptr = allocator.allocate(as: Win32EventLoopExecutor.Timestamp.self)
      } else {
        unsafe ptr = .allocate(capacity: 1)
      }
      unsafe self.win32TimestampPointer = ptr
    }
  }

  fileprivate mutating func clearWin32Timestamp() {
    // If we allocated the Timestamp, deallocate
    if win32TimestampIsIndirect {
      let ptr = unsafe self.win32TimestampPointer
      if let allocator {
        unsafe allocator.deallocate(ptr)
      } else {
        unsafe ptr.deallocate()
      }
    }
  }
}

/// The Win32EventLoopExecutor delegate protocol.
///
/// The delegate protocol allows programs to inject their own code into the
/// event loop, for instance to process messages for non-modal dialogues.
public protocol Win32EventLoopExecutorDelegate {

  /// Called before the event loop calls TranslateMessage()
  ///
  /// Parameters:
  ///
  /// - message:  The message being processed.
  ///
  /// Returns `true` to prevent the event loop from processing the message
  /// further, for instance if the `preTranslateMessage` function has
  /// completely handled the message somehow.
  func preTranslateMessage(_ message: inout MSG) -> Bool

}

/// Retrieve a message from the Win32 message queue
///
/// This exists to work around the incorrect return type declared by the
/// GetMessage() API, which claims to return BOOL, but really returns an
/// INT (0, 1, or -1).
fileprivate func GetMessage(_ message: inout MSG,
                            _ hWnd: HWND?,
                            _ wMsgFilterMin: UINT,
                            _ wMsgFilterMax: UINT) -> CInt {
  let getMessagePtr = unsafe GetMessageW
  let getMessage = unsafe unsafeBitCast(
    getMessagePtr,
    to: ((LPMSG?, HWND?, UINT, UINT) -> CInt).self)
  return unsafe getMessage(&message, hWnd, wMsgFilterMin, wMsgFilterMax)
}

/// An executor that uses a Windows event loop
@available(SwiftStdlib 6.2, *)
@safe
public final class Win32EventLoopExecutor
  : SerialExecutor, RunLoopExecutor, @unchecked Sendable
{

  struct Timestamp {
    /// The earliest time at which a job should run.
    ///
    /// Jobs will never be run earlier than this.
    var target: UInt64

    /// The maximum (ideal) tolerable delay.
    ///
    /// We make no guarantee that we won't run over this, but it is taken
    /// into consideration when scheduling jobs.
    var leeway: UInt64

    /// The latest time at which a job should (ideally) run.
    ///
    /// We may run the job after this point, but we will not run other jobs
    /// with later deadlines before this job.
    var deadline: UInt64 {
      get {
        if UInt64.max - target < leeway {
          return UInt64.max
        }
        return target + leeway
      }
      set {
        if newValue < leeway {
          target = 0
        } else {
          target = newValue - leeway
        }
      }
    }
  }

  private enum WaitQueue: Int {
    case continuous = 0
    case suspending = 1
  }

  private let waitQueues: Mutex<[PriorityQueue<UnownedJob>]>
  private let runQueue: Mutex<PriorityQueue<UnownedJob>>
  private var currentRunQueue: PriorityQueue<UnownedJob>

  private var dwThreadId: DWORD?
  private var hEvent: HANDLE!
  private let bShouldStop: Atomic<Bool>
  private let sequence: Atomic<UInt>

  private(set) public var isMainExecutor: Bool

  var delegate: (any Win32EventLoopExecutorDelegate)?

  public init(isMainExecutor: Bool = false) {
    self.isMainExecutor = isMainExecutor
    self.dwThreadId = nil
    self.bShouldStop = Atomic<Bool>(false)
    self.sequence = Atomic<UInt>(0)
    guard let hEvent = unsafe CreateEventW(nil, true, false, nil) else {
      let dwError = GetLastError()
      fatalError("Unable to create event: \(String(dwError, radix: 16))")
    }
    unsafe self.hEvent = hEvent

    let comparePriorities = { (lhs: UnownedJob, rhs: UnownedJob) -> Bool in
      if lhs.priority == rhs.priority {
        // If they're the same priority, compare the sequence numbers to
        // ensure this queue gives stable ordering.  We want the lowest
        // sequence number first, but note that we want to handle wrapping.
        let delta = ExecutorJob(lhs).win32Sequence
          &- ExecutorJob(rhs).win32Sequence
        return (delta >> (UInt.bitWidth - 1)) != 0
      }
      return lhs.priority > rhs.priority
    }

    self.runQueue = Mutex(PriorityQueue(compare: comparePriorities))
    self.currentRunQueue = PriorityQueue(compare: comparePriorities)

    let compareTimestamps = { (lhs: UnownedJob, rhs: UnownedJob) -> Bool in
      return ExecutorJob(lhs).win32Timestamp.deadline
        < ExecutorJob(rhs).win32Timestamp.deadline
    }

    self.waitQueues = Mutex(
      [
        PriorityQueue(compare: compareTimestamps),
        PriorityQueue(compare: compareTimestamps)
      ]
    )
  }

  deinit {
    unsafe CloseHandle(hEvent)
  }

  private func wakeEventLoop() {
    let bRet = unsafe SetEvent(hEvent)
    if !bRet {
      let dwError = GetLastError()
      fatalError("SetEvent() failed while trying to wake event loop: error 0x\(String(dwError, radix: 16))")
    }
  }

  /// Run the event loop.
  ///
  /// This method runs a Win32 event loop.
  public func run() throws {
    // Make sure we're running on the same thread every time
    let dwCurrentThreadId = GetCurrentThreadId()
    if dwThreadId == nil {
      dwThreadId = dwCurrentThreadId
    }

    precondition(dwThreadId == dwCurrentThreadId,
                 "Win32EventLoopExecutor must always run on the same thread")

    while true {
      // Switch queues
      runQueue.withLock {
        swap(&$0, &currentRunQueue)
        unsafe ResetEvent(hEvent)
      }

      // Move jobs from the timer queue to the run queue as needed
      fireTimerQueues()

      // Run anything in the run queue at this point
      while let job = currentRunQueue.pop() {
        unsafe ExecutorJob(job).runSynchronously(
          on: self.asUnownedSerialExecutor()
        )
      }

      // Work out how long to wait for
      let dwMsToWait = computeNextTimerWait()

      // Wait for messages, the stop event, or APCs
      let dwRet = unsafe MsgWaitForMultipleObjectsEx(
        1, &hEvent, dwMsToWait,
        DWORD(QS_ALLINPUT),
        DWORD(MWMO_ALERTABLE)
      )

      // The event is signalled when one of the following is true:
      //
      // * A new job was queued for excecution.
      // * A new job was scheduled for later excecution.
      // * The stop() method has been called.
      //
      if dwRet == WAIT_OBJECT_0 {
        if bShouldStop.load(ordering: .acquiring) {
          bShouldStop.store(false, ordering: .relaxed)
          break
        }
      }

      // If we have a message waiting, process it
      if dwRet == WAIT_OBJECT_0 + 1 {
        var msg = unsafe MSG()

        let bRet = unsafe GetMessage(&msg, nil, 0, 0)

        if bRet == -1 {
          let dwError = GetLastError()
          fatalError("GetMessage() failed: error 0x\(String(dwError, radix: 16))")
        }

        if bRet == 0 {
          // We received WM_QUIT, so exit; note that we re-post the quit
          // message, in case we're nested somehow.  It doesn't matter too
          // much if this is the outermost loop - we'll just quit at that
          // point.
          PostQuitMessage(CInt(unsafe msg.wParam))
          break
        }

        var skipMessageProcessing = false
        if let delegate {
          skipMessageProcessing = unsafe delegate.preTranslateMessage(&msg)
        }

        if !skipMessageProcessing {
          unsafe TranslateMessage(&msg)
          unsafe DispatchMessageW(&msg)
        }
      }

    }
  }

  /// Signal to the event loop to stop running and return.
  public func stop() {
    bShouldStop.store(true, ordering: .releasing)
    wakeEventLoop()
  }

  /// Enqueue a job.
  ///
  /// Parameters:
  ///
  /// - job:   The job to schedule.
  ///
  public func enqueue(_ job: consuming ExecutorJob) {
    // Tag it with a sequence number to force ordering for same-priority jobs
    let (newSequence, _) = sequence.wrappingAdd(1, ordering: .relaxed)
    job.win32Sequence = newSequence

    let unownedJob = UnownedJob(job)
    runQueue.withLock {
      $0.push(unownedJob)
    }

    wakeEventLoop()
  }

  /// Process the timer queues.
  ///
  /// We maintain two timer queues, one for continuous time, and one
  /// for suspending time.  The difference is that one of them is
  /// driven by `QueryInterruptTimePrecise()`, while the other is driven
  /// by `QueryUnbiasedInterruptTimePrecise()`.
  ///
  /// This function will move jobs whose timers have elapsed to the run
  /// queue.
  func fireTimerQueues() {
    var now: [UInt64] = [0, 0]

    unsafe QueryInterruptTimePrecise(&now[WaitQueue.continuous.rawValue])
    unsafe QueryUnbiasedInterruptTimePrecise(&now[WaitQueue.suspending.rawValue])

    waitQueues.withLock { queues in
      for queue in WaitQueue.continuous.rawValue...WaitQueue.suspending.rawValue {
        // Run all of the queued events
        while let job = queues[queue].pop(
                when: {
                  ExecutorJob($0).win32Timestamp.target <= now[queue]
                }
              ) {
          var theJob = ExecutorJob(job)
          theJob.clearWin32Timestamp()
          currentRunQueue.push(job)
        }
      }
    }
  }

  /// Work out how long we should wait for on the next loop.
  ///
  /// Looking at the timer queues, work out how long we want to wait for
  /// events on the next pass around the loop.
  ///
  /// Returns the number of milliseconds to tell Windows to wait.
  func computeNextTimerWait() -> DWORD {
    var now: [UInt64] = [0, 0]

    unsafe QueryInterruptTimePrecise(&now[WaitQueue.continuous.rawValue])
    unsafe QueryUnbiasedInterruptTimePrecise(&now[WaitQueue.suspending.rawValue])

    // Find the smallest time we need to wait
    var lowestDelta: UInt64?
    var leeway: UInt64?

    waitQueues.withLock { queues in
      for queue in WaitQueue.continuous.rawValue...WaitQueue.suspending.rawValue {
        if let job = queues[queue].top {
          let timestamp = ExecutorJob(job).win32Timestamp
          let delta = timestamp.deadline - now[queue]

          if lowestDelta == nil {
            lowestDelta = delta
            leeway = timestamp.leeway
          } else if delta < lowestDelta! {
            lowestDelta = delta
            leeway = timestamp.leeway
          }
        }
      }
    }

    // If there's nothing to wait for, return INFINITE
    guard let lowestDelta, let leeway else {
      return INFINITE
    }

    // Compute the desired fire time
    let fireTime = lowestDelta - leeway

    // Convert to milliseconds
    var msToWait = fireTime / 10000

    // If we have less than 15ms of leeway, reduce `msToWait` so that
    // we spin up to the fire time.
    //
    // The reason for this is that Windows waits in an unusual way; it actually
    // keeps track of time in 15.625ms (1/64s) "ticks", and if you ask to wait
    // for less than one tick it will run an inaccurate delay loop.  Further,
    // because of when it decrements the tick count, it may actually return by
    // up to one tick *early*, depending on when in a tick you ask to wait.
    if leeway < 156250 {
      msToWait -= min(msToWait, 15)
    }

    // INFINITE is 0xffffffff (see <WinBase.h>)
    if msToWait >= INFINITE {
      msToWait = UInt64(INFINITE - 1)
    }

    return DWORD(truncatingIfNeeded: msToWait)
  }

  public var asSchedulable: SchedulableExecutor? {
    return self
  }
}

@available(SwiftStdlib 6.2, *)
extension Win32EventLoopExecutor: SchedulableExecutor {

  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                after delay: C.Duration,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    let queue: WaitQueue
    if clock.traits.contains(.continuous) {
      queue = .continuous
    } else {
      queue = .suspending
    }

    var now: UInt64 = 0
    switch queue {
      case .continuous:
        unsafe QueryUnbiasedInterruptTimePrecise(&now)
      case .suspending:
        unsafe QueryInterruptTimePrecise(&now)
    }

    let delayAsDuration = clock.convert(from: delay)!
    let (delaySecs, delayAttos) = delayAsDuration.components
    let delay100ns = delaySecs * 10000000 + delayAttos / 100000000000
    let tolerance100ns: Int64
    if let tolerance {
      let toleranceAsDuration = clock.convert(from: tolerance)!
      let (toleranceSecs, toleranceAttos) = toleranceAsDuration.components
      tolerance100ns = toleranceSecs * 10000000 + toleranceAttos / 100000000000
    } else {
      // Default tolerance is 10%, with a maximum of 100ms and a minimum of
      // 15.625ms (so as not to needlessly trigger spinning for accuracy).
      tolerance100ns = max(min(delay100ns / 10, 1000000), 156250)
    }

    let timestamp = Timestamp(target: now + UInt64(delay100ns),
                              leeway: UInt64(tolerance100ns))

    job.setupWin32Timestamp()
    job.win32Timestamp = timestamp

    let unownedJob = UnownedJob(job)
    waitQueues.withLock { queues in
      queues[queue.rawValue].push(unownedJob)
    }
    wakeEventLoop()
  }

}

extension Win32EventLoopExecutor: MainExecutor {}

/// An executor that uses a Win32 thread pool.
@available(SwiftStdlib 6.2, *)
@safe
public final class Win32ThreadPoolExecutor: TaskExecutor, @unchecked Sendable {

  private var cbeHighPriority = unsafe TP_CALLBACK_ENVIRON()
  private var cbeLowPriority = unsafe TP_CALLBACK_ENVIRON()
  private var cbeNormalPriority = unsafe TP_CALLBACK_ENVIRON()

  /// Construct a Win32ThreadPoolExecutor.
  ///
  /// This is a convenience initializer to avoid having to write unsafe
  /// in the normal case where you want to use the default thread pool.
  public convenience init() {
    unsafe self.init(pool: nil)
  }

  /// Construct a Win32ThreadPoolExecutor.
  ///
  /// Parameters:
  ///
  /// - pool:  The thread pool to use; `nil` means use the default thread
  ///          pool.
  ///
  public init(pool: PTP_POOL?) {
    unsafe InitializeThreadpoolEnvironment(&cbeHighPriority)
    unsafe InitializeThreadpoolEnvironment(&cbeLowPriority)
    unsafe InitializeThreadpoolEnvironment(&cbeNormalPriority)

    unsafe SetThreadpoolCallbackPriority(&cbeHighPriority,
                                         TP_CALLBACK_PRIORITY_HIGH)
    unsafe SetThreadpoolCallbackPriority(&cbeLowPriority,
                                         TP_CALLBACK_PRIORITY_LOW)
    unsafe SetThreadpoolCallbackPriority(&cbeNormalPriority,
                                         TP_CALLBACK_PRIORITY_NORMAL)

    if let pool = unsafe pool {
      unsafe SetThreadpoolCallbackPool(&cbeHighPriority, pool)
      unsafe SetThreadpoolCallbackPool(&cbeLowPriority, pool)
      unsafe SetThreadpoolCallbackPool(&cbeNormalPriority, pool)
    }
  }

  deinit {
    unsafe DestroyThreadpoolEnvironment(&cbeHighPriority)
    unsafe DestroyThreadpoolEnvironment(&cbeLowPriority)
    unsafe DestroyThreadpoolEnvironment(&cbeNormalPriority)
  }

  private func withEnvironment<R>(
    for priority: JobPriority,
    body: (UnsafeMutablePointer<TP_CALLBACK_ENVIRON>) -> R
  ) -> R {
    if priority.rawValue <= TaskPriority.low.rawValue {
      return unsafe withUnsafeMutablePointer(to: &cbeLowPriority, body)
    } else if priority.rawValue >= TaskPriority.high.rawValue {
      return unsafe withUnsafeMutablePointer(to: &cbeHighPriority, body)
    } else {
      return unsafe withUnsafeMutablePointer(to: &cbeNormalPriority, body)
    }
  }

  /// Enqueue a job.
  ///
  /// Parameters:
  ///
  /// - job:   The job to schedule.
  public func enqueue(_ job: consuming ExecutorJob) {
    unsafe job.win32ThreadPoolExecutor = self.asUnownedTaskExecutor()

    let priority = job.priority
    let unownedJob = UnownedJob(job)
    let work = unsafe withEnvironment(for: priority) { environment in
      unsafe CreateThreadpoolWork(_runJobOnThreadPool,
                                  unsafe unsafeBitCast(unownedJob,
                                                       to: PVOID.self),
                                  environment)
    }

    unsafe SubmitThreadpoolWork(work)
    unsafe CloseThreadpoolWork(work)
  }

  public var asSchedulable: SchedulableExecutor? {
    return self
  }
}

@_cdecl("_swift_runJobOnThreadPool")
fileprivate func _runJobOnThreadPool(
  instance: PTP_CALLBACK_INSTANCE?,
  context: UnsafeMutableRawPointer?,
  work: PTP_WORK?
) {
  let job = unsafe unsafeBitCast(context, to: UnownedJob.self)
  let executor = unsafe ExecutorJob(job).win32ThreadPoolExecutor
  unsafe job.runSynchronously(on: executor)
}

@_cdecl("_swift_runJobFromTimerCallback")
fileprivate func _runJobFromTimerCallback(
  instance: PTP_CALLBACK_INSTANCE?,
  context: UnsafeMutableRawPointer?,
  timer: PTP_TIMER?
) {
  let job = unsafe unsafeBitCast(context, to: UnownedJob.self)
  let executor = unsafe ExecutorJob(job).win32ThreadPoolExecutor
  unsafe job.runSynchronously(on: executor)
  unsafe CloseThreadpoolTimer(timer)
}

@available(SwiftStdlib 6.2, *)
extension Win32ThreadPoolExecutor: SchedulableExecutor {

  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                after delay: C.Duration,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    let delayAsDuration = clock.convert(from: delay)!
    let (delaySecs, delayAttos) = delayAsDuration.components
    let delay100ns = delaySecs * 10000000 + delayAttos / 100000000000

    var fireTime: FILETIME

    // The thread pool timers can either do a suspending delay (that is, the
    // time spent asleep does not count), *or* an absolute time, so to do
    // a delay from a continuous clock, we calculate the expected fire time
    // from the delay and the current time.
    if clock.traits.contains(.continuous) {
      var now = FILETIME(dwLowDateTime: 0, dwHighDateTime: 0)
      unsafe GetSystemTimePreciseAsFileTime(&now)

      let now100ns = UInt64(now.dwLowDateTime) | UInt64(now.dwHighDateTime) << 32
      let target100ns = now100ns + UInt64(delay100ns)

      fireTime = FILETIME(dwLowDateTime:
                            DWORD(truncatingIfNeeded: target100ns),
                          dwHighDateTime:
                            DWORD(truncatingIfNeeded: target100ns >> 32))
    } else {
      fireTime = FILETIME(dwLowDateTime:
                            DWORD(truncatingIfNeeded: -delay100ns),
                          dwHighDateTime:
                            DWORD(truncatingIfNeeded: -delay100ns >> 32))
    }

    unsafe job.win32ThreadPoolExecutor = self.asUnownedTaskExecutor()

    let priority = job.priority
    let unownedJob = UnownedJob(job)
    let timer = unsafe withEnvironment(for: priority) { environment in
      unsafe CreateThreadpoolTimer(
        _runJobFromTimerCallback,
        unsafe unsafeBitCast(unownedJob,
                             to: UnsafeMutableRawPointer.self),
        environment
      )
    }

    guard let timer = unsafe timer else {
      let dwError = GetLastError()
      fatalError("unable to create Win32 thread pool timer: error 0x\(String(dwError, radix: 16))")
    }

    let msWindowLength: DWORD
    if let tolerance {
      let toleranceAsDuration = clock.convert(from: tolerance)!
      let (toleranceSecs, toleranceAttos) = toleranceAsDuration.components
      msWindowLength = DWORD(toleranceSecs * 1000)
        + DWORD(toleranceAttos / 1_000_000_000_000_000)
      //                                 ^ns     ^us ^ms
    } else {
      // Default tolerance is 10%, with a maximum of 100ms and a minimum of
      // 15ms, same as for the event loop.
      msWindowLength = max(min(DWORD(delay100ns / 100000), 100), 15)
    }

    unsafe SetThreadpoolTimer(timer, &fireTime, 0, msWindowLength)
  }

}

#endif // os(Windows)
