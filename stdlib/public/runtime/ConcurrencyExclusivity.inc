//===--- ConcurrencyExclusivity.cpp - Exclusivity tracking for concurrency-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This implements the runtime support for dynamically tracking exclusivity
// in tasks.
//
//===----------------------------------------------------------------------===//

namespace {

/// High Level Algorithm Description
/// --------------------------------
///
/// With the introduction of Concurrency, we add additional requirements to our
/// exclusivity model:
///
/// * We want tasks to have a consistent exclusivity access set across
///   suspensions/resumptions. This ensures that any exclusive accesses began
///   before a Task suspended are properly flagged after the Task is resumed
///   even if the Task is resumed on a different thread.
///
/// * If a synchronous code calls a subroutine that creates a set of tasks to
///   perform work and then blocks, we want the runtime to ensure that the tasks
///   respect exclusivity accesses from the outside synchronous code.
///
/// * We on purpose define exclusive access to the memory from multiple tasks as
///   undefined behavior since that would be an additional feature that needs to
///   be specifically designed in the future.
///
/// * We assume that an access in synchronous code will never be ended in
///   asynchronous code.
///
/// * We additional require that our design leaves the exclusivity runtime
///   unaware of any work we are doing here. All it should be aware of is the
///   current thread local access set and adding/removing from that access set.
///
/// We implement these requirements by reserving two pointers in each Task. The
/// first pointer points at the head access of the linked list of accesses of
/// the Task and the second pointer points at the end of the linked list of
/// accesses of the task. We will for the discussion ahead call the first
/// pointer TaskFirstAccess and the second TaskLastAccess. This allows us to
/// modify the current TLV single linked list to include/remove the tasks’s
/// access by updating a few nodes in the linked list when the task is running
/// and serialize the task’s current access set and restoring to be head the
/// original synchronous access set head when the task is running. This
/// naturally fits a push/pop access set sort of schema where every time a task
/// starts, we push its access set onto the local TLV and then pop it off when
/// the task is suspended. This ensures that the task gets the current
/// synchronous set of accesses and other Tasks do not see the accesses of the
/// specific task providing task isolation.
///
/// The cases can be described via the following table:
///
/// +------+--------------------+--------------------+--------------------+
/// | Case | Live Task Accesses | Live Sync Accesses | Live Task Accesses |
/// |      | When Push          | When Push          | When Pop           |
/// |------+--------------------+--------------------+--------------------|
/// |    1 | F                  | F                  | F                  |
/// |    2 | F                  | F                  | T                  |
/// |    3 | F                  | T                  | F                  |
/// |    4 | F                  | T                  | T                  |
/// |    5 | T                  | F                  | F                  |
/// |    6 | T                  | F                  | T                  |
/// |    7 | T                  | T                  | F                  |
/// |    8 | T                  | T                  | T                  |
/// +------+--------------------+--------------------+--------------------+
///
/// We mark the end of each title below introducing a case with 3 T/F to enable
/// easy visual matching with the chart
///
/// Case 1: Task/Sync do not have initial accesses and no Task accesses are
/// created while running (F,F,F)
///
/// In this case, TBegin and TEnd are both initially nullptr.
///
/// When we push, we see that the current exclusivity TLV has a null head and
/// leave it so. We set TBegin and TEnd as nullptr while running.
///
/// When we pop, see that the exclusivity TLV is still nullptr, so we just leave
/// TBegin and TEnd alone still as nullptr.
///
/// This means that code that does not have any exclusive accesses do not have
/// any runtime impact.
///
/// Case 2: Task/Sync do not have initial access, but Task accesses are created
/// while running (F, F, T)
///
/// In this case, TBegin and TEnd are both initially nullptr.
///
/// When we push, we see that the current exclusivity TLV has a null head. So,
/// we leave TBegin and TEnd as nullptr while the task is running.
///
/// When we pop, we see that the exclusivity TLV has a non-null head. In that
/// case, we walk the list to find the last node and update TBegin to point at
/// the current head, TEnd to point at that last node, and then set the TLV head
/// to be nullptr.
///
/// Case 3: Task does not have initial accesses, but Sync does, and new Task
/// accesses are not created while running (F, T, F)
///
/// In this case, TBegin and TEnd are both initially nullptr.
///
/// When we push, we look at the TLV and see our initial synchronous thread was
/// tracking accesses. In this case, we leave the TLV pointing at the
/// SyncAccessHead and set TBegin to SyncAccessHead and leave TEnd as nullptr.
///
/// When we pop, we see that TBegin (which we know has the old synchronous head
/// in it) is equal to the TLV so we know that we did not create any new Task
/// accesses. Then we set TBegin to nullptr and return. NOTE:  TEnd is nullptr
/// the entire time in this scenario.
///
/// Case 4: Task does not have initial accesses, but Sync does, and new Task
/// accesses are created while running (F, T, T)
///
/// In this case, TBegin and TEnd are both initially nullptr. When we push, we
/// look at the TLV and we see our initial synchronous thread was tracking
/// accesses. In this case, we leave the TLV pointing at the SyncAccessHead and
/// set TBegin to SyncAccessHead and leave TEnd as nullptr.
///
/// When we pop, we see that the TLV and TBegin differ now. We know that this
/// means that our task introduced new accesses. So, we search down from the
/// head of the AccessSet TLV until we find TBegin . The node before TBegin is
/// our new TEnd pointer. We set TBegin to then have the value of head, TEnd to
/// be the new TEnd pointer, set TEnd’s next to be nullptr and make head the old
/// value of TBegin.
///
/// Case 5: Task has an initial access set, but Sync does not have initial
/// accesses and no Task accesses exist after running (T,F,F)
///
/// In this case, TBegin and TEnd are both initially set to non-null values.
/// When we push, we look at the current TLV head and see that the TLV head is
/// nullptr. We then set TLV head to be TBegin and set TBegin to be nullptr to
/// signal the original synchronous TLV head was nullptr.
///
/// When we pop, we see that TBegin is currently nullptr, so we know the
/// synchronous access set was empty. We also know that despite us starting with
/// a task access set, those accesses must have completed while the task was
/// running since the access set is empty when we pop.
///
/// Case 6: Task has initial accesses, sync does not have initial access, and
/// Task access set is modified while running (T, F, T)
///
/// In this case, TBegin and TEnd are both initially set to non-null
/// values. When we push, we look at the current TLV head and see that the TLV
/// head is nullptr. We then set TLV head to be TBegin and set TBegin to be
/// nullptr to signal the original synchronous TLV head was nullptr. We have no
/// requirement on TEnd now in this case but set it to nullptr, to track flags
/// if we want to in the future in a different runtime.
///
/// When we pop, we see that TBegin is currently nullptr, so we know the
/// synchronous access set was empty. We do not have a way to know how/if we
/// modified the Task AccessSet, so we walked the list to find the last node. We
/// then make TBegin head, TEnd the last node, and set the TLV to be nullptr
/// again.
///
/// Case 7: Task has initial accesses, Sync has initial accesses, and new Task
/// accesses are not created while running (T, T, F)
///
/// In this case, TBegin and TEnd are both initially set to non-null values.
/// When we push, we look at the current TLV head and see that the TLV head is a
/// valid pointer. We then set TLV head to be the current value of TBegin, make
/// TEnd->next the old head value and stash the old head value into TBegin. We
/// have no requirement on TEnd now in this case.
///
/// When we pop, we see that TBegin is not nullptr, so we know the synchronous
/// access set had live accesses. We do not have a way to know how/if we
/// modified the Task AccessSet, so we walked the list to find TBegin (which is
/// old sync head).  Noting that the predecessor node of old sync head’s node
/// will be the end of the task’s current access set, we set TLV to point at the
/// node we found in TBegin, set TBegin to the current TLV head, set TEnd to
/// that predecessor node of the current TLV head and set TEnd->next to be
/// nullptr.
///
/// Case 8: Task has initial accesses, Sync does, and Task accesses is modified
/// while running (T, T, T)
///
/// In this case, TBegin and TEnd are both initially set to non-null values.
///
/// When we push, we look at the current TLV head and see that the TLV head is
/// a valid pointer. We then set TLV head to be the current value of TBegin,
/// make TEnd->next the old head value and stash the old head value into
/// TBegin. We have no requirement on TEnd now in this case.
///
/// When we pop, we see that TBegin is not nullptr, so we know the synchronous
/// access set had live accesses. We do not have a way to know how/if we
/// modified the Task AccessSet, so we walked the list to find TBegin (which is
/// old sync head).  Noting that the predecessor node of old sync head’s node
/// will be the end of the task’s current access set, we set TLV to point at
/// the node we found in TBegin, set TBegin to the current TLV head, set TEnd
/// to that predecessor node of the current TLV head and set TEnd->next to be
/// nullptr.
struct SwiftTaskThreadLocalContext {
  uintptr_t state[2];

#ifndef NDEBUG
  void dump() {
    fprintf(stderr,
            "        SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): "
            "(%p, %p)\n",
            (void *)state[0], (void *)state[1]);
  }
#endif

  bool hasInitialAccessSet() const {
    // If state[0] is nullptr, we have an initial access set.
    return bool(state[0]);
  }

  Access *getTaskAccessSetHead() const {
    return reinterpret_cast<Access *>(state[0]);
  }

  Access *getTaskAccessSetTail() const {
    return reinterpret_cast<Access *>(state[1]);
  }

  void setTaskAccessSetHead(Access *newHead) { state[0] = uintptr_t(newHead); }

  void setTaskAccessSetTail(Access *newTail) { state[1] = uintptr_t(newTail); }

#ifndef NDEBUG
  const char *getTaskAddress() const {
    // Constant only used when we have an asserts compiler so that we can output
    // exactly the header location of the task for FileCheck purposes.
    //
    // WARNING: This test will fail if the Task ABI changes. When that happens,
    // update the offset!
    //
    // TODO: This probably will need 32 bit help.
#if __POINTER_WIDTH__ == 64
    unsigned taskHeadOffsetFromTaskAccessSet = 128;
#else
    unsigned taskHeadOffsetFromTaskAccessSet = 68;
#endif
    auto *self = reinterpret_cast<const char *>(this);
    return self - taskHeadOffsetFromTaskAccessSet;
  }
#endif
};

} // end anonymous namespace

// See algorithm description on SwiftTaskThreadLocalContext.
void swift::swift_task_enterThreadLocalContext(char *state) {
  auto &taskCtx = *reinterpret_cast<SwiftTaskThreadLocalContext *>(state);
  auto &tlsCtxAccessSet = SwiftTLSContext::get().accessSet;

#ifndef NDEBUG
  if (isExclusivityLoggingEnabled()) {
    withLoggingLock([&]() {
      fprintf(stderr,
              "Entering Thread Local Context. Before Swizzle. Task: %p\n",
              taskCtx.getTaskAddress());
      taskCtx.dump();
      swift_dumpTrackedAccesses();
    });
  }

  auto logEndState = [&] {
    if (isExclusivityLoggingEnabled()) {
      withLoggingLock([&]() {
        fprintf(stderr,
                "Entering Thread Local Context. After Swizzle. Task: %p\n",
                taskCtx.getTaskAddress());
        taskCtx.dump();
        swift_dumpTrackedAccesses();
      });
    }
  };
#else
  // Just a no-op that should inline away.
  auto logEndState = [] {};
#endif

  // First handle all of the cases where our task does not start without an
  // initial access set.
  //
  // Handles push cases 1-4.
  if (!taskCtx.hasInitialAccessSet()) {
    // In this case, the current synchronous context is not tracking any
    // accesses. So the tlsCtx and our initial access set are all nullptr, so we
    // can just return early.
    //
    // Handles push cases 1-2.
    if (!tlsCtxAccessSet) {
      logEndState();
      return;
    }

    // Ok, our task isn't tracking any task specific accesses, but our tlsCtx
    // was tracking accesses. Leave the tlsCtx alone at this point and set our
    // state's begin access to be tlsCtx head. We leave our access set tail as
    // nullptr.
    //
    // Handles push cases 3-4.
    taskCtx.setTaskAccessSetHead(tlsCtxAccessSet.getHead());
    logEndState();
    return;
  }

  // At this point, we know that we did have an initial access set. Both access
  // set pointers are valid.
  //
  // Handles push cases 5-8.

  // Now check if our synchronous code had any accesses. If not, we set TBegin,
  // TEnd to be nullptr and set the tlsCtx to point to TBegin.
  //
  // Handles push cases 5-6.
  if (!bool(tlsCtxAccessSet)) {
    tlsCtxAccessSet = taskCtx.getTaskAccessSetHead();
    taskCtx.setTaskAccessSetHead(nullptr);
    taskCtx.setTaskAccessSetTail(nullptr);
    logEndState();
    return;
  }

  // In this final case, we found that our task had its own access set and our
  // tlsCtx did as well. So we then set the Task's head to be the new TLV head,
  // set tail->next to point at old head and stash oldhead into the task ctx.
  //
  // Handles push cases 7-8.
  auto *oldHead = tlsCtxAccessSet.getHead();
  auto *tail = taskCtx.getTaskAccessSetTail();

  tlsCtxAccessSet.setHead(taskCtx.getTaskAccessSetHead());
  tail->setNext(oldHead);
  taskCtx.setTaskAccessSetHead(oldHead);
  taskCtx.setTaskAccessSetTail(nullptr);
  logEndState();
}

// See algorithm description on SwiftTaskThreadLocalContext.
void swift::swift_task_exitThreadLocalContext(char *state) {
  auto &taskCtx = *reinterpret_cast<SwiftTaskThreadLocalContext *>(state);
  auto &tlsCtxAccessSet = SwiftTLSContext::get().accessSet;

#ifndef NDEBUG
  if (isExclusivityLoggingEnabled()) {
    withLoggingLock([&]() {
      fprintf(stderr,
              "Exiting Thread Local Context. Before Swizzle. Task: %p\n",
              taskCtx.getTaskAddress());
      taskCtx.dump();
      swift_dumpTrackedAccesses();
    });
  }

  auto logEndState = [&] {
    if (isExclusivityLoggingEnabled()) {
      withLoggingLock([&]() {
        fprintf(stderr,
                "Exiting Thread Local Context. After Swizzle. Task: %p\n",
                taskCtx.getTaskAddress());
        taskCtx.dump();
        swift_dumpTrackedAccesses();
      });
    }
  };
#else
  // If we are not compiling with asserts, just use a simple identity function
  // that should be inlined away.
  //
  // TODO: Can we use defer in the runtime?
  auto logEndState = [] {};
#endif

  // First check our ctx to see if we were tracking a previous synchronous
  // head. If we don't then we know that our synchronous thread was not
  // initially tracking any accesses.
  //
  // Handles pop cases 1,2,5,6
  Access *oldHead = taskCtx.getTaskAccessSetHead();
  if (!oldHead) {
    // Then check if we are currently tracking an access set in the TLS. If we
    // aren't, then we know that either we did not start with a task specific
    // access set /or/ we did start but all of those accesses ended while the
    // task was running. In either case, when we pushed initially, we set
    // TBegin, TEnd to be nullptr already and since oldHead is already nullptr,
    // we can just exit.
    //
    // Handles pop cases 1,5
    if (!tlsCtxAccessSet) {
      assert(taskCtx.getTaskAccessSetTail() == nullptr &&
             "Make sure we set this to nullptr when we pushed");
      logEndState();
      return;
    }

    // In this case, we did find that we had live accesses. Since we know we
    // did not start with any synchronous accesses, these accesses must all be
    // from the given task. So, we first find the tail of the current TLS linked
    // list, then set the Task access set head to accessSet, the Task accessSet
    // tail to the TLS linked list tail and set tlsCtx.accessSet to nullptr.
    //
    // Handles pop cases 2,6
    auto *newHead = tlsCtxAccessSet.getHead();
    auto *newTail = tlsCtxAccessSet.getTail();
    assert(newTail && "Failed to find tail?!");
    tlsCtxAccessSet = nullptr;
    taskCtx.setTaskAccessSetHead(newHead);
    taskCtx.setTaskAccessSetTail(newTail);
    logEndState();
    return;
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
  // Handles pop cases 3.
  if (tlsCtxAccessSet.getHead() == oldHead) {
    taskCtx.setTaskAccessSetHead(nullptr);
    taskCtx.setTaskAccessSetTail(nullptr);
    logEndState();
    return;
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
  auto *newHead = tlsCtxAccessSet.getHead();
  auto *newEnd = tlsCtxAccessSet.findParentAccess(oldHead);
  tlsCtxAccessSet.setHead(oldHead);
  newEnd->setNext(nullptr);
  taskCtx.setTaskAccessSetHead(newHead);
  taskCtx.setTaskAccessSetTail(newEnd);
  logEndState();
}
