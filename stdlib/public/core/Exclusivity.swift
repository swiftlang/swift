//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Dynamic exclusivity checking.
///
//===----------------------------------------------------------------------===//

import SwiftShims

fileprivate let ValueBufferSize = unsafe 3 * MemoryLayout<UnsafeRawPointer>.stride

fileprivate let TrackingFlag: UInt = 0x20
fileprivate let ActionMask: UInt = 0x1

typealias AccessPointer = UnsafeMutablePointer<Access>
@unsafe struct Access {

  enum Action: UInt {
    case read
    case modify
  }

  struct NextAndAction {
    private var rawValue: UInt

    var action: Action {
      get {
        Action(rawValue: rawValue & ActionMask)!
      }
      set {
        rawValue = (rawValue & ~ActionMask) | newValue.rawValue
      }
    }

    var next: AccessPointer? {
      get { unsafe UnsafeMutablePointer(bitPattern: rawValue & ~ActionMask) }
      set { rawValue = UInt(bitPattern: newValue) | (rawValue & ActionMask) }
    }
  }

  var location: UnsafeRawPointer?
  var pc: UnsafeRawPointer?
  var nextAndAction: NextAndAction

  var action: Action {
    get { unsafe nextAndAction.action }
    set { unsafe nextAndAction.action = newValue }
  }

  var next: AccessPointer? {
    get { unsafe nextAndAction.next }
    set { unsafe nextAndAction.next = newValue }
  }

  static func from(rawPointer: UnsafeMutableRawPointer?) -> AccessPointer? {
    guard let rawPointer = unsafe rawPointer else { return nil }
    return unsafe rawPointer.assumingMemoryBound(to: Access.self)
  }

  @inline(__always)
  static func search(
    buffer: UnsafeMutableRawPointer,
    location: UnsafeRawPointer,
    pc: UnsafeRawPointer?,
    action: Action,
    inserting: Bool,
    head: inout AccessPointer?
  ) {
    var cursor = unsafe head
    while let nextPtr = unsafe cursor {
      if unsafe nextPtr.pointee.location == location {
        if unsafe nextPtr.pointee.action == .modify || action == .modify {
          #if !$Embedded
          unsafe _swift_reportExclusivityConflict(
            nextPtr.pointee.action.rawValue,
            nextPtr.pointee.pc,
            action.rawValue,
            pc,
            location)
          #else
          unsafe _embeddedReportExclusivityViolation(
            oldAction: nextPtr.pointee.action,
            oldPC: nextPtr.pointee.pc,
            newAction: action,
            newPC: pc,
            pointer: location)
          #endif
        }
      }
      unsafe cursor = nextPtr.pointee.next
    }

    if inserting {
      guard let access = unsafe Access.from(rawPointer: buffer) else {
        nullAccessBuffer()
      }

      unsafe access.pointee.location = location
      unsafe access.pointee.pc = pc
      unsafe access.pointee.action = action

      unsafe access.pointee.next = head
      unsafe head = access
    }
  }

  @inline(__always)
  static func remove(access: AccessPointer, head: inout AccessPointer?) {
    var cursor = unsafe head
    var previous: AccessPointer? = nil
    while let nextPtr = unsafe cursor {
      if unsafe nextPtr == access {
        if let previous = unsafe previous {
          unsafe previous.pointee.next = access.pointee.next
        } else {
          unsafe head = access.pointee.next
        }
        return
      }
      unsafe previous = nextPtr
      unsafe cursor = nextPtr.pointee.next
    }

    unsafe accessNotFound(access)
  }

  @inline(__always)
  static func findParent(
    access: AccessPointer,
    child: AccessPointer?
  ) -> AccessPointer? {
    var cursor = unsafe access
    while let next = unsafe cursor.pointee.next {
      if unsafe next == child {
        return unsafe cursor
      }
      unsafe cursor = next
    }

    // If we were searching for nil, then we found it.
    if unsafe child == nil {
      return unsafe cursor
    }

    // If we were searching for a non-nil node, we didn't find it.
    return nil
  }

  static func forEach(_ head: AccessPointer?, _ action: (Access) -> Void) {
    var cursor = unsafe head
    while let nextPtr = unsafe cursor {
      unsafe action(nextPtr.pointee)
      unsafe cursor = nextPtr.pointee.next
    }
  }
}

fileprivate var accessHead: AccessPointer? {
  get { unsafe Access.from(rawPointer: _swift_getExclusivityTLS()) }
  set { unsafe _swift_setExclusivityTLS(newValue) }
}

@c(swift_beginAccess)
@usableFromInline
@unsafe
internal func swift_beginAccess(
  pointer: UnsafeRawPointer,
  buffer: UnsafeMutableRawPointer,
  flags: UInt,
  pc: UnsafeRawPointer?
) {
  // Make sure that Access fits in the value buffer. This should be a static
  // assert, but we'll settle for a precondition for now. The optimizer should
  // eliminate the check when it's true.
  precondition(unsafe MemoryLayout<Access>.size <= ValueBufferSize)

  #if !$Embedded
  // If exclusivity checking is disabled, mark this access as untracked by
  // putting nil into the access buffer's pointer field, and return.
  if _swift_disableExclusivityChecking {
    unsafe Access.from(rawPointer: buffer)?.pointee.location = nil
    return
  }
  #endif

  guard let action = Access.Action(rawValue: flags & ActionMask) else {
    invalidFlags(flags)
  }

  let isTracking = (flags & TrackingFlag) != 0

  unsafe Access.search(
    buffer: buffer,
    location: pointer,
    pc: pc ?? _swift_stdlib_get_return_address(),
    action: action,
    inserting: isTracking,
    head: &accessHead)
}

@c(swift_endAccess)
@usableFromInline
@unsafe
internal func swift_endAccess(buffer: UnsafeMutableRawPointer) {
  guard let access = unsafe Access.from(rawPointer: buffer) else {
    nullAccessBuffer()
  }

  // If the pointer field is nil then we're not tracking this access.
  if unsafe access.pointee.location == nil {
    return
  }
  unsafe Access.remove(access: access, head: &accessHead)
}

#if INTERNAL_CHECKS_ENABLED
@c(swift_dumpTrackedAccesses)
@usableFromInline
@unsafe
@_unavailableInEmbedded
internal func swift_dumpTrackedAccesses() {
  if let head = unsafe accessHead {
    unsafe Access.forEach(head) {
      unsafe _swift_stdlib_fputs_stderr("        Access. " +
          "Pointer: \($0.location, default: "<null>")." +
          "PC: \($0.pc, default: "<null>"). " +
          "AccessAction: \($0.action)\n")
    }
  } else {
    unsafe _swift_stdlib_fputs_stderr("        No Accesses.\n")
  }
}
#endif

//===----------------------------------------------------------------------===//
// Exclusivity tracking across task suspension
//===----------------------------------------------------------------------===//
//
// High Level Algorithm Description
// --------------------------------
//
// With the introduction of Concurrency, we add additional requirements to our
// exclusivity model:
//
// * We want tasks to have a consistent exclusivity access set across
//   suspensions/resumptions. This ensures that any exclusive accesses began
//   before a Task suspended are properly flagged after the Task is resumed
//   even if the Task is resumed on a different thread.
//
// * If synchronous code calls a subroutine that creates a set of tasks to
//   perform work and then blocks, we want the runtime to ensure that the tasks
//   respect exclusivity accesses from the outside synchronous code.
//
// * We on purpose define exclusive access to the memory from multiple tasks as
//   undefined behavior since that would be an additional feature that needs to
//   be specifically designed in the future.
//
// * We assume that an access in synchronous code will never be ended in
//   asynchronous code.
//
// * We additional require that our design leaves the exclusivity runtime
//   unaware of any work we are doing here. All it should be aware of is the
//   current thread local access set and adding/removing from that access set.
//
// We implement these requirements by reserving two pointers in each Task. The
// first pointer points at the head access of the linked list of accesses of
// the Task and the second pointer points at the end of the linked list of
// accesses of the task. We will for the discussion ahead call the first
// pointer TaskFirstAccess and the second TaskLastAccess. This allows us to
// modify the current TLV single linked list to include/remove the tasks’s
// access by updating a few nodes in the linked list when the task is running
// and serialize the task’s current access set and restoring to be head the
// original synchronous access set head when the task is running. This
// naturally fits a push/pop access set sort of schema where every time a task
// starts, we push its access set onto the local TLV and then pop it off when
// the task is suspended. This ensures that the task gets the current
// synchronous set of accesses and other Tasks do not see the accesses of the
// specific task providing task isolation.
//
// The cases can be described via the following table:
//
// +------+--------------------+--------------------+--------------------+
// | Case | Live Task Accesses | Live Sync Accesses | Live Task Accesses |
// |      | When Push          | When Push          | When Pop           |
// |------+--------------------+--------------------+--------------------|
// |    1 | F                  | F                  | F                  |
// |    2 | F                  | F                  | T                  |
// |    3 | F                  | T                  | F                  |
// |    4 | F                  | T                  | T                  |
// |    5 | T                  | F                  | F                  |
// |    6 | T                  | F                  | T                  |
// |    7 | T                  | T                  | F                  |
// |    8 | T                  | T                  | T                  |
// +------+--------------------+--------------------+--------------------+
//
// We mark the end of each title below introducing a case with 3 T/F to enable
// easy visual matching with the chart
//
// Case 1: Task/Sync do not have initial accesses and no Task accesses are
// created while running (F,F,F)
//
// In this case, TBegin and TEnd are both initially nil.
//
// When we push, we see that the current exclusivity TLV has a null head and
// leave it so. We set TBegin and TEnd as nil while running.
//
// When we pop, see that the exclusivity TLV is still nil, so we just leave
// TBegin and TEnd alone still as nil.
//
// This means that code that does not have any exclusive accesses do not have
// any runtime impact.
//
// Case 2: Task/Sync do not have initial access, but Task accesses are created
// while running (F, F, T)
//
// In this case, TBegin and TEnd are both initially nil.
//
// When we push, we see that the current exclusivity TLV has a null head. So, we
// leave TBegin and TEnd as nil while the task is running.
//
// When we pop, we see that the exclusivity TLV has a non-null head. In that
// case, we walk the list to find the last node and update TBegin to point at
// the current head, TEnd to point at that last node, and then set the TLV head
// to be nil.
//
// Case 3: Task does not have initial accesses, but Sync does, and new Task
// accesses are not created while running (F, T, F)
//
// In this case, TBegin and TEnd are both initially nil.
//
// When we push, we look at the TLV and see our initial synchronous thread was
// tracking accesses. In this case, we leave the TLV pointing at the
// SyncAccessHead and set TBegin to SyncAccessHead and leave TEnd as nil.
//
// When we pop, we see that TBegin (which we know has the old synchronous head
// in it) is equal to the TLV so we know that we did not create any new Task
// accesses. Then we set TBegin to nil and return. NOTE: TEnd is nil the entire
// time in this scenario.
//
// Case 4: Task does not have initial accesses, but Sync does, and new Task
// accesses are created while running (F, T, T)
//
// In this case, TBegin and TEnd are both initially nil. When we push, we look
// at the TLV and we see our initial synchronous thread was tracking accesses.
// In this case, we leave the TLV pointing at the SyncAccessHead and set TBegin
// to SyncAccessHead and leave TEnd as nil.
//
// When we pop, we see that the TLV and TBegin differ now. We know that this
// means that our task introduced new accesses. So, we search down from the head
// of the AccessSet TLV until we find TBegin . The node before TBegin is our new
// TEnd pointer. We set TBegin to then have the value of head, TEnd to be the
// new TEnd pointer, set TEnd’s next to be nil and make head the old value of
// TBegin.
//
// Case 5: Task has an initial access set, but Sync does not have initial
// accesses and no Task accesses exist after running (T,F,F)
//
// In this case, TBegin and TEnd are both initially set to non-null values. When
// we push, we look at the current TLV head and see that the TLV head is nil. We
// then set TLV head to be TBegin and set TBegin to be nil to signal the
// original synchronous TLV head was nil.
//
// When we pop, we see that TBegin is currently nil, so we know the synchronous
// access set was empty. We also know that despite us starting with a task
// access set, those accesses must have completed while the task was running
// since the access set is empty when we pop.
//
// Case 6: Task has initial accesses, sync does not have initial access, and
// Task access set is modified while running (T, F, T)
//
// In this case, TBegin and TEnd are both initially set to non-null values. When
// we push, we look at the current TLV head and see that the TLV head is nil. We
// then set TLV head to be TBegin and set TBegin to be nil to signal the
// original synchronous TLV head was nil. We have no requirement on TEnd now in
// this case but set it to nil, to track flags if we want to in the future in a
// different runtime.
//
// When we pop, we see that TBegin is currently nil, so we know the synchronous
// access set was empty. We do not have a way to know how/if we modified the
// Task AccessSet, so we walked the list to find the last node. We then make
// TBegin head, TEnd the last node, and set the TLV to be nil again.
//
// Case 7: Task has initial accesses, Sync has initial accesses, and new Task
// accesses are not created while running (T, T, F)
//
// In this case, TBegin and TEnd are both initially set to non-null values. When
// we push, we look at the current TLV head and see that the TLV head is a valid
// pointer. We then set TLV head to be the current value of TBegin, make
// TEnd->next the old head value and stash the old head value into TBegin. We
// have no requirement on TEnd now in this case.
//
// When we pop, we see that TBegin is not nil, so we know the synchronous access
// set had live accesses. We do not have a way to know how/if we modified the
// Task AccessSet, so we walked the list to find TBegin (which is old sync
// head).  Noting that the predecessor node of old sync head’s node will be the
// end of the task’s current access set, we set TLV to point at the node we
// found in TBegin, set TBegin to the current TLV head, set TEnd to that
// predecessor node of the current TLV head and set TEnd->next to be nil.
//
// Case 8: Task has initial accesses, Sync does, and Task accesses is modified
// while running (T, T, T)
//
// In this case, TBegin and TEnd are both initially set to non-null values.
//
// When we push, we look at the current TLV head and see that the TLV head is a
// valid pointer. We then set TLV head to be the current value of TBegin, make
// TEnd->next the old head value and stash the old head value into TBegin. We
// have no requirement on TEnd now in this case.
//
// When we pop, we see that TBegin is not nil, so we know the synchronous access
// set had live accesses. We do not have a way to know how/if we modified the
// Task AccessSet, so we walked the list to find TBegin (which is old sync
// head).  Noting that the predecessor node of old sync head’s node will be the
// end of the task’s current access set, we set TLV to point at the node we
// found in TBegin, set TBegin to the current TLV head, set TEnd to that
// predecessor node of the current TLV head and set TEnd->next to be nil.

#if !$Embedded

/// Wrapper around the reserved exclusivity space in a Task. The reserved space
/// is two pointers, which we use as two AccessPointer? values representing head
/// and tail.
@unsafe
private struct TaskExclusivityContext {
  var state: UnsafeMutablePointer<AccessPointer?>

  var head: AccessPointer? {
    get { unsafe state[0] }
    nonmutating set { unsafe state[0] = newValue }
  }

  var tail: AccessPointer? {
    get { unsafe state[1] }
    nonmutating set { unsafe state[1] = newValue }
  }

  init(_ ptr: UnsafeMutableRawPointer) {
    unsafe state = ptr.assumingMemoryBound(to: AccessPointer?.self)
  }
}

/// Push the task's exclusivity access set onto the thread-local list. Called
/// when a task initializes or resumes. `state` points at the task's two-word
/// `(head, tail)` buffer.
@c(swift_task_enterThreadLocalContext)
@usableFromInline
@unsafe
internal func swift_task_enterThreadLocalContext(
  state: UnsafeMutableRawPointer
) {
  let ctx = unsafe TaskExclusivityContext(state)
  let tlsHead = unsafe accessHead

  // First handle all of the cases where our task does not start without an
  // initial access set.
  //
  // Handles push cases 1-4.
  if unsafe ctx.head == nil {
    // In this case, the current synchronous context is not tracking any
    // accesses. So the tlsCtx and our initial access set are all nullptr, so we
    // can just return early.
    //
    // Handles push cases 1-2.
    guard let tlsHead = unsafe tlsHead else { return }

    // Ok, our task isn't tracking any task specific accesses, but our tlsCtx
    // was tracking accesses. Leave the tlsCtx alone at this point and set our
    // state's begin access to be tlsCtx head. We leave our access set tail as
    // nullptr.
    //
    // Handles push cases 3-4.
    unsafe ctx.head = tlsHead
    return
  }

  // The task has its own access set. Handles push cases 5-8.

  // At this point, we know that we did have an initial access set. Both access
  // set pointers are valid.
  //
  // Handles push cases 5-8.

  // Now check if our synchronous code had any accesses. If not, we set head and
  // tail to be nullptr and set the tlsCtx to point to head.
  //
  // Handles push cases 5-6.
  guard let tlsHead = unsafe tlsHead else {
    unsafe accessHead = ctx.head
    unsafe ctx.head = nil
    unsafe ctx.tail = nil
    return
  }

  // In this final case, we found that our task had its own access set and our
  // tlsCtx did as well. So we then set the Task's head to be the new TLV head,
  // set tail->next to point at old head and stash oldhead into the task ctx.
  //
  // Handles push cases 7-8.
  let oldHead = unsafe tlsHead
  let tail = unsafe ctx.tail
  unsafe accessHead = ctx.head
  unsafe tail?.pointee.next = oldHead
  unsafe ctx.head = oldHead
  unsafe ctx.tail = nil
}

/// Pop the task's exclusivity access set back out of the thread-local list,
/// restoring the synchronous head. Called when a task suspends. `state` points
/// at the task's two-word `(head, tail)` buffer.
@c(swift_task_exitThreadLocalContext)
@usableFromInline
@unsafe
internal func swift_task_exitThreadLocalContext(
  state: UnsafeMutableRawPointer
) {
  let ctx = unsafe TaskExclusivityContext(state)
  let tlsHead = unsafe accessHead

  // First check our ctx to see if we were tracking a previous synchronous
  // head. If we don't then we know that our synchronous thread was not
  // initially tracking any accesses.
  //
  // Handles pop cases 1,2,5,6
  guard let oldHead = unsafe ctx.head else {
    // Then check if we are currently tracking an access set in the TLS. If we
    // aren't, then we know that either we did not start with a task specific
    // access set /or/ we did start but all of those accesses ended while the
    // task was running. In either case, when we pushed initially, we set
    // TBegin, TEnd to be nullptr already and since oldHead is already nullptr,
    // we can just exit.
    //
    // Handles pop cases 1,5
    guard let tlsHead = unsafe tlsHead else {
      _internalInvariant(
        unsafe ctx.tail == nil,
        "tail should have been nil since push")
      return
    }

    // In this case, we did find that we had live accesses. Since we know we
    // did not start with any synchronous accesses, these accesses must all be
    // from the given task. So, we first find the tail of the current TLS linked
    // list, then set the Task access set head to accessSet, the Task accessSet
    // tail to the TLS linked list tail and set tlsCtx.accessSet to nullptr.
    //
    // Handles pop cases 2,6
    let newTail = unsafe Access.findParent(access: tlsHead, child: nil)
    _internalInvariant(unsafe newTail != nil, "Failed to find tail?!")
    unsafe accessHead = nil
    unsafe ctx.head = tlsHead
    unsafe ctx.tail = newTail
    return
  }

  // Otherwise, we know that we /were/ tracking accesses from a previous
  // synchronous context. So we need to unmerge our task specific state from the
  // exclusivity access set.
  //
  // Handles pop cases 3,4,7,8.

  // First check if the current head tlsAccess is the same as our oldHead. In
  // such a case, we do not have new task accesses to update. So just set task
  // access head/tail to nullptr. The end access should be nullptr.
  //
  // Handles pop case 3.
  if unsafe tlsHead == oldHead {
    unsafe ctx.head = nil
    unsafe ctx.tail = nil
    return
  }

  // Otherwise, we have task specific accesses that we need to serialize into
  // the task's state. We currently can not tell if the Task actually modified
  // the task list beyond if the task list is empty. So we have to handle case 7
  // here (unfortunately).
  //
  // NOTE: If we could tell if the Task modified its access set while running,
  // we could perhaps avoid the search for newEnd.
  //
  // Handles pop cases 4,7,8.
  let newHead = unsafe tlsHead
  var newEnd: AccessPointer? = nil
  if let tlsHead = unsafe tlsHead {
    unsafe newEnd = Access.findParent(access: tlsHead, child: oldHead)
  }
  unsafe accessHead = oldHead
  unsafe newEnd?.pointee.next = nil
  unsafe ctx.head = newHead
  unsafe ctx.tail = newEnd
}

#endif

@inline(never)
fileprivate func invalidFlags(_ flags: UInt) -> Never {
  #if !$Embedded
  reportExclusivityError("Internal exclusivity error",
    "unable to construct action from flags \(flags)")
  #else
  fatalError("Internal exclusivity error: unable to construct action from flags")
  #endif
}

@inline(never)
fileprivate func accessNotFound(_ access: AccessPointer) -> Never {
  #if !$Embedded
  unsafe reportExclusivityError("Internal exclusivity error",
                                "didn't find exclusive access buffer \(access)")
  #else
  fatalError("Internal exclusivity error: didn't find exclusive access buffer")
  #endif
}

@inline(never)
fileprivate func nullAccessBuffer() -> Never {
  #if !$Embedded
  reportExclusivityError("Internal exclusivity error", "NULL access buffer")
  #else
  fatalError("Internal exclusivity error: NULL access buffer")
  #endif
}

@_unavailableInEmbedded
fileprivate func reportExclusivityError(
  _ prefix: StaticString, _ message: String
) -> Never {
  prefix.withUTF8Buffer { prefixBuffer in
    var message = message
    message.withUTF8 { messageBuffer in
      #if !$Embedded
      unsafe _swift_stdlib_reportFatalError(
          prefixBuffer.baseAddress!, CInt(prefixBuffer.count),
          messageBuffer.baseAddress!, CInt(messageBuffer.count),
          0)
      #else
      unsafe _embeddedReportFatalError(
          prefix: prefix,
          message: messageBuffer)
      #endif
    }
  }
  Builtin.int_trap()
}
