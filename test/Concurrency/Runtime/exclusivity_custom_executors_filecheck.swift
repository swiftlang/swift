// REQUIRES: rdar81024581
// RUN: export env %env-SWIFT_DEBUG_RUNTIME_EXCLUSIVITY_LOGGING=1 && \
// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) 2>&1 | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: use_os_stdlib

// Disabled until test hang can be looked at.
// UNSUPPORTED: OS=windows-msvc

// Only enabled if our stdlib has asserts enabled since the exclusivity runtime
// will only emit logging when the stdlib is compiled with asserts. This is done
// on purpose since we do not want to ship the runtime with this logging even
// possible.
//
// UNSUPPORTED: swift_stdlib_no_asserts

// This test makes sure that we properly save/restore access when we
// synchronously launch a task from a serial executor. The access from the task
// should be merged into the already created access set while it runs and then
// unmerged afterwards.

import _Concurrency
import Dispatch

// For fputs, file lock.
#if canImport(CRT)

import CRT

@inlinable
public func lockStderr() {
    _lock_file(stderr)
}
@inlinable
public func unlockStderr() {
    _unlock_file(stderr)
}

#else

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

@inlinable
public func lockStderr() {
    flockfile(stderr)
}

@inlinable
public func unlockStderr() {
    funlockfile(stderr)
}

#endif

@inlinable
public func debugLog(_ s: String) {
    lockStderr()
    fputs(s + "\n", stderr)
    fflush(stderr)
    unlockStderr()
}

@inline(never)
public func withExclusiveAccess<T, U>(to x: inout T, f: (inout T) -> U) -> U {
    debugLog("==> Enter 'withExclusiveAccess'")
    defer { debugLog("==> Exit 'withExclusiveAccess'") }
    return f(&x)
}

@available(SwiftStdlib 5.5, *)
@MainActor @inline(never)
func withExclusiveAccessAsync<T, U>(to x: inout T, f: (inout T) async -> U) async -> U {
    debugLog("==> Enter 'withExclusiveAccessAsync'")
    defer { debugLog("==> Exit 'withExclusiveAccessAsync'") }
    return await f(&x)
}

@available(SwiftStdlib 5.5, *)
public final class MySerialExecutor : SerialExecutor {
    public init() {
        debugLog("==> MySerialExecutor: Creating MySerialExecutor!")
    }
    public static var sharedSerialExecutor = MySerialExecutor()
    public static var sharedUnownedExecutor: UnownedSerialExecutor {
        debugLog("==> MySerialExecutor: Getting Shared Unowned Executor!")
        return UnownedSerialExecutor(ordinary: sharedSerialExecutor)
    }

    public func enqueue(_ job: UnownedJob) {
        debugLog("==> MySerialExecutor: Got an enqueue!")
        // This is the exclusive access that we are going to be swizzling
        // in/out.
        //
        // Make sure we have 2x synchronized to test.
        withExclusiveAccess(to: &global2) { _ in
            withExclusiveAccess(to: &global3) { _ in
                debugLog("==> MySerialExecutor: Inside access!")
                job._runSynchronously(on: asUnownedSerialExecutor())
                debugLog("==> MySerialExecutor: Inside access after run synchronously!")
            }
        }
    }

    public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
        debugLog("==> MySerialExecutor: Getting Unowned Executor!")
        return UnownedSerialExecutor(ordinary: self)
    }
}

/// A singleton actor whose executor is equivalent to the main
/// dispatch queue.
@available(SwiftStdlib 5.5, *)
@globalActor public final actor MyMainActor: Executor {
    public static let shared = MyMainActor()
    public let executor = MySerialExecutor()

    @inlinable
    public nonisolated var unownedExecutor: UnownedSerialExecutor {
        debugLog("==> MyMainActor: Getting unowned exector!")
        return executor.asUnownedSerialExecutor()
    }

    @inlinable
    public static var sharedUnownedExecutor: UnownedSerialExecutor {
        debugLog("==> MyMainActor: Getting shared unowned exector!")
        return MySerialExecutor.sharedUnownedExecutor
    }

    @inlinable
    public nonisolated func enqueue(_ job: UnownedJob) {
        debugLog("==> MyMainActor: enqueuing!")
        executor.enqueue(job)
    }
}

/// An actor that we use to test that after eliminating the synchronous
/// accesses, we properly deserialize the task access set causing a crash in
/// unownedExecutor.
@available(SwiftStdlib 5.5, *)
@globalActor public final actor MyMainActorWithAccessInUnownedExecAccessor: Executor {
    public static let shared = MyMainActorWithAccessInUnownedExecAccessor()
    public let executor = MySerialExecutor()

    @inlinable
    public nonisolated var unownedExecutor: UnownedSerialExecutor {
        debugLog("==> MyMainActorWithAccessInUnownedExecAccessor: Getting unowned exector!")
        withExclusiveAccess(to: &global) { _ in debugLog("Crash!") }
        return executor.asUnownedSerialExecutor()
    }

    @inlinable
    public static var sharedUnownedExecutor: UnownedSerialExecutor {
        debugLog("==> MyMainActorWithAccessInUnownedExecAccessor: Getting shared unowned exector!")
        return MySerialExecutor.sharedUnownedExecutor
    }

    @inlinable
    public nonisolated func enqueue(_ job: UnownedJob) {
        debugLog("==> MyMainActorWithAccessInUnownedExecAccessor: enqueuing!")
        executor.enqueue(job)
    }
}

@available(SwiftStdlib 5.5, *)
actor Custom {
    var count = 0

    func report() async {
        debugLog("==> Custom: custom.count == \(count)")
        count += 1
    }
}

@available(SwiftStdlib 5.5, *)
@globalActor
struct CustomActor {
    static var shared: Custom {
        debugLog("==> CustomActor: Getting custom!")
        return Custom()
    }
}

public var global: Int = 5
public var global2: Int = 6
public var global3: Int = 7
public var global4: Int = 8

@available(SwiftStdlib 5.5, *)
@main
struct Runner {

    // CHECK-LABEL: ==> Enter 'mergeSyncAccessesIntoTaskAccesses'
    // CHECK: ==> MySerialExecutor: Got an enqueue!
    // CHECK-NEXT: Inserting new access: [[SYNC_LLNODE_1:0x[0-9a-f]+]]
    // CHECK-NEXT:   Tracking!
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1:0x[0-9a-f]+]]. PC:
    // CHECK-NEXT: ==> Enter 'withExclusiveAccess'
    // CHECK-NEXT: Inserting new access: [[SYNC_LLNODE_2:0x[0-9a-f]+]]
    // CHECK-NEXT:   Tracking!
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2:0x[0-9a-f]+]].
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK-NEXT: ==> Enter 'withExclusiveAccess'
    // CHECK-NEXT: ==> MySerialExecutor: Inside access!
    // CHECK-NEXT: ==> MySerialExecutor: Getting Unowned Executor!
    // CHECK-NEXT: Entering Thread Local Context. Before Swizzle. Task: [[TASK:0x[0-9a-f]+]]
    // CHECK-NEXT:         SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): (0x0, 0x0)
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK-NEXT: Entering Thread Local Context. After Swizzle. Task: [[TASK]]
    // CHECK-NEXT:         SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): ([[SYNC_LLNODE_2]], 0x0)
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK: ==> Exit 'mergeSyncAccessesIntoTaskAccesses'
    @MainActor
    static func mergeSyncAccessesIntoTaskAccesses() async {
        debugLog("==> Enter 'mergeSyncAccessesIntoTaskAccesses'")
        defer { debugLog("==> Exit 'mergeSyncAccessesIntoTaskAccesses'") }

        debugLog("==> Before handle")
        let handle = Task { @MyMainActor in
            debugLog("==> Main: In handle!")
            debugLog("==> No Crash!")
            debugLog("==> Main: All done!")
        }
        await handle.value
    }

    // If all of the previous tests passed, then we have basic sanity
    // done. Lets now test out our cases that involve a live sync access.
    //
    // We test cases 3,4,7,8 here. The other cases that do not involve a
    // custom executor are tested in Runtime/exclusivity.swift.

    // Now that we have tested our tests with various numbers of accesses,
    // lets make specific tests for each case in Exclusivity.cpp.
    //
    // Case 3: (F, T, F) - No Live Accesses at Task Start, Exiting Live Sync
    // Accesses When Push, No Live Task Accesses when pop.
    //
    // This case is the case where we do not have any accesses in our code
    // at all or if the task cleans up the tasks before it awaits again. We
    // test the task cleanup case.

    // CHECK-LABEL: ==> Enter 'testCase3'
    // CHECK: ==> MySerialExecutor: Got an enqueue!
    // CHECK-NEXT: Inserting new access: [[SYNC_LLNODE_1:0x[0-9a-f]+]]
    // CHECK-NEXT:   Tracking!
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1:0x[0-9a-f]+]].
    // CHECK-NEXT: ==> Enter 'withExclusiveAccess'
    // CHECK-NEXT: Inserting new access: [[SYNC_LLNODE_2:0x[0-9a-f]+]]
    // CHECK-NEXT:   Tracking!
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK: ==> Enter 'withExclusiveAccess'
    // CHECK: ==> MySerialExecutor: Inside access!
    // CHECK: ==> MySerialExecutor: Getting Unowned Executor!
    // CHECK-NEXT: Entering Thread Local Context. Before Swizzle. Task: [[TASK_1:0x[0-9a-f]+]]
    // CHECK-NEXT:         SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): (0x0, 0x0)
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK-NEXT: Entering Thread Local Context. After Swizzle. Task: [[TASK_1]]
    // CHECK-NEXT:         SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): ([[SYNC_LLNODE_2]], 0x0)
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK: ==> Main: In handle!
    // CHECK: ==> No Crash!
    // CHECK: ==> Main: All done!
    // CHECK: Inserting new access: [[TASK_LLNODE:0x[0-9a-f]+]]
    // CHECK-NEXT:   Tracking!
    // CHECK-NEXT:         Access. Pointer: [[TASK_ACCESS:0x[0-9a-f]+]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK-NEXT: ==> Enter 'withExclusiveAccess'
    // CHECK-NEXT: ==> Making sure can push/pop access
    // CHECK-NEXT: ==> Exit 'withExclusiveAccess'
    // CHECK-NEXT: Removing access: [[TASK_LLNODE]]
    // CHECK: Exiting Thread Local Context. Before Swizzle. Task: [[TASK_1]]
    // CHECK-NEXT:         SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): ([[SYNC_LLNODE_2]], 0x0)
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK: Exiting Thread Local Context. After Swizzle. Task: [[TASK_1]]
    // CHECK-NEXT:         SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): (0x0, 0x0)
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK: ==> Exit 'testCase3'
    @MainActor
    static func testCase3() async {
        debugLog("==> Enter 'testCase3'")
        defer { debugLog("==> Exit 'testCase3'") }

        debugLog("==> Before handle")
        let handle = Task { @MyMainActor in
            debugLog("==> Main: In handle!")
            debugLog("==> No Crash!")
            debugLog("==> Main: All done!")

            withExclusiveAccess(to: &global) { _ in
                debugLog("==> Making sure can push/pop access")
            }
            // In order to test that we properly hand off the access, we
            // need to await here.
            let handle2 = await Task { @CustomActor in
                debugLog("==> In inner handle")
            }
            await handle2.value
        }
        await handle.value
    }

    // Case 4: (F, T, T). In case 4, our task does not start with a live
    // access but we have accesses from the outside synchronous context, and
    // do add new accesses when we pop.
    //
    // CHECK-LABEL: ==> Enter 'testCase4'
    // CHECK: ==> MySerialExecutor: Got an enqueue!
    // CHECK-NEXT: Inserting new access: [[SYNC_NODE_1:0x[0-9a-f]+]]
    // CHECK-NEXT:   Tracking!
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1:0x[0-9a-f]+]]. PC:
    // CHECK: ==> Enter 'withExclusiveAccess'
    // CHECK-NEXT: Inserting new access: [[SYNC_NODE_2:0x[0-9a-f]+]]
    // CHECK-NEXT:   Tracking!
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK: ==> Enter 'withExclusiveAccess'
    // CHECK: ==> MySerialExecutor: Inside access!
    // CHECK: ==> MySerialExecutor: Getting Unowned Executor!
    // CHECK-NEXT: Entering Thread Local Context. Before Swizzle. Task: [[TASK:0x[0-9a-f]+]]
    // CHECK-NEXT:         SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): (0x0, 0x0)
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK-NEXT: Entering Thread Local Context. After Swizzle. Task: [[TASK]]
    // CHECK-NEXT:         SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): ([[SYNC_NODE_2]], 0x0)
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK: ==> Main: In handle!
    // CHECK: ==> No Crash!
    // CHECK: ==> Main: All done!
    // CHECK: Inserting new access: [[TASK_NODE:0x[0-9a-f]+]]
    // CHECK-NEXT:   Tracking!
    // CHECK-NEXT:         Access. Pointer: [[TASK_ACCESS:0x[0-9a-f]+]].
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK-NEXT: Exiting Thread Local Context. Before Swizzle. Task: [[TASK]]
    // CHECK-NEXT:         SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): ([[SYNC_NODE_2]], 0x0)
    // CHECK-NEXT:         Access. Pointer: [[TASK_ACCESS]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK-NEXT: Exiting Thread Local Context. After Swizzle. Task: [[TASK]]
    // CHECK-NEXT:         SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): ([[TASK_NODE]], [[TASK_NODE]])
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK: ==> MySerialExecutor: Inside access after run synchronously!
    // CHECK: ==> Exit 'testCase4'
    @MainActor
    static func testCase4() async {
        debugLog("==> Enter 'testCase4'")
        defer { debugLog("==> Exit 'testCase4'") }

        debugLog("==> Before handle")
        let handle = Task { @MyMainActor in
            debugLog("==> Main: In handle!")
            debugLog("==> No Crash!")
            debugLog("==> Main: All done!")

            await withExclusiveAccessAsync(to: &global) {
                @MyMainActor (x: inout Int) async -> Void in
                debugLog("==> Making sure can push/pop access")
            }
            // In order to test that we properly hand off the access, we
            // need to await here.
            let handle2 = await Task { @CustomActor in
                debugLog("==> In inner handle")
            }
            await handle2.value
        }
        await handle.value
    }

    // Case 7. (T, T, F). In case 7, our task starts with live accesses and
    // sync accesses, but the live accesses are popped before we return.
    @MyMainActor @inline(never)
    static func withExclusiveAccessAsyncCase7<T, U>(to x: inout T, f: (inout T) async -> U) async -> U {
        debugLog("==> Enter 'withExclusiveAccessAsyncCase7'")
        defer { debugLog("==> Exit 'withExclusiveAccessAsyncCase7'") }
        let t = Task { @MainActor in
            debugLog("==> Task to force serialization of MyMainActor by using MainActor")
        }
        await t.value
        return await f(&x)
    }

    // CHECK-LABEL: ==> Enter 'testCase7'
    // CHECK: ==> MySerialExecutor: Got an enqueue!
    // CHECK: ==> MySerialExecutor: Got an enqueue!
    // CHECK-NEXT: Inserting new access: [[SYNC_NODE_1:0x[0-9a-f]+]]
    // CHECK-NEXT:   Tracking!
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1:0x[0-9a-f]+]]. PC:
    // CHECK-NEXT: ==> Enter 'withExclusiveAccess'
    // CHECK-NEXT: Inserting new access: [[SYNC_NODE_2:0x[0-9a-f]+]]
    // CHECK-NEXT:   Tracking!
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2:0x[0-9a-f]+]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK: ==> Enter 'withExclusiveAccess'
    // CHECK: ==> MySerialExecutor: Inside access!
    // CHECK: ==> MySerialExecutor: Getting Unowned Executor!
    // CHECK: Entering Thread Local Context. Before Swizzle. Task: [[TASK:0x[0-9a-f]+]]
    // CHECK-NEXT:         SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): ([[TASK_NODE:0x[0-9a-f]+]], [[TASK_NODE]])
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK-NEXT: Entering Thread Local Context. After Swizzle. Task: [[TASK]]
    // CHECK-NEXT:         SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): ([[SYNC_NODE_2]], 0x0)
    // CHECK-NEXT:         Access. Pointer: [[TASK_ACCESS:0x[0-9a-f]+]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK: ==> MyMainActor: Getting unowned exector!
    // CHECK: ==> MySerialExecutor: Getting Unowned Executor!
    // CHECK: ==> Making sure can push/pop access
    // CHECK: ==> Exit 'withExclusiveAccessAsyncCase7'
    // CHECK-NEXT: Removing access: [[TASK_NODE]]
    // CHECK: Exiting Thread Local Context. Before Swizzle. Task: [[TASK]]
    // CHECK-NEXT:         SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): ([[SYNC_NODE_2]], 0x0)
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK: Exiting Thread Local Context. After Swizzle. Task: [[TASK]]
    // CHECK-NEXT:         SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): (0x0, 0x0)
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_2]]. PC:
    // CHECK-NEXT:         Access. Pointer: [[SYNC_ACCESS_1]]. PC:
    // CHECK: ==> Exit 'testCase7'
    @MainActor
    static func testCase7() async {
        debugLog("==> Enter 'testCase7'")
        defer { debugLog("==> Exit 'testCase7'") }

        debugLog("==> Before handle")
        let handle = Task { @MyMainActor in
            debugLog("==> Main: In handle!")
            debugLog("==> No Crash!")
            debugLog("==> Main: All done!")

            await withExclusiveAccessAsyncCase7(to: &global) {
                @MyMainActor (x: inout Int) async -> Void in
                debugLog("==> Making sure can push/pop access")
            }

            // In order to test that we properly hand off the access, we
            // need to await here.
            let handle2 = await Task { @CustomActor in
                debugLog("==> In inner handle")
            }
            await handle2.value
        }
        await handle.value
    }

    // Case 8. (T, T, T). In case 8, our task starts with live accesses and
    // sync accesses, and we have remaining live accesses when we return.
    @MyMainActor @inline(never)
    static func withExclusiveAccessAsyncCase8<T, U>(to x: inout T, f: (inout T) async -> U) async -> U {
        debugLog("==> Enter 'withExclusiveAccessAsyncCase8'")
        defer { debugLog("==> Exit 'withExclusiveAccessAsyncCase8'") }
        let t = Task { @MainActor in
            debugLog("==> Task1 to force serialization of MyMainActor by using MainActor")
        }
        await t.value

        await f(&x)

        let t2 = Task { @MainActor in
            debugLog("==> Task2 to force serialization of MyMainActor by using MainActor")
        }
        await t2.value

        return await f(&x)
    }

    @MainActor
    static func testCase8() async {
        debugLog("==> Enter 'testCase8'")
        defer { debugLog("==> Exit 'testCase8'") }

        debugLog("==> Before handle")
        let handle = Task { @MyMainActor in
            debugLog("==> Main: In handle!")
            debugLog("==> No Crash!")
            debugLog("==> Main: All done!")

            await withExclusiveAccessAsyncCase8(to: &global) {
                @MyMainActor (x: inout Int) async -> Void in
                debugLog("==> Making sure can push/pop access")
            }

            // In order to test that we properly hand off the access, we
            // need to await here.
            let handle2 = await Task { @CustomActor in
                debugLog("==> In inner handle")
            }
            await handle2.value
        }
        await handle.value
    }

    @MainActor static func main() async {
        await mergeSyncAccessesIntoTaskAccesses()
        await testCase3()
        await testCase4()
        await testCase7()
        await testCase8()
    }
}
