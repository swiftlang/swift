// RUN: export env SWIFT_DEBUG_RUNTIME_EXCLUSIVITY_LOGGING=1 && %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library) 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// Only enabled if our stdlib has asserts enabled since the exclusivity runtime
// will only emit logging when the stdlib is compiled with asserts. This is done
// on purpose since we do not want to ship the runtime with this logging even
// possible.
//
// UNSUPPORTED: swift_stdlib_no_asserts

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// This test makes sure that:
//
// 1. Tasks have separate exclusivity sets.
// 2. Exercise the pushing/popping of access sets from tasks.

// NOTE: The cases that we are talking about handling below refer to the cases
// documented in Exclusivity.cpp.

import _Concurrency
import StdlibUnittest

var global1: Int = 5
var global2: Int = 6
var global3: Int = 7

@available(SwiftStdlib 5.5, *)
@main
struct Runner {
    @inline(never)
    @MainActor
    static func doSomething() async { }

    @inline(never)
    @Sendable
    static func useGlobal(_ x: inout Int) { print("FORCE ACCESS") }

    // Test specifically that we properly maintain different access sets when
    // using different tasks. We are not trying to specifically test any of the
    // cases here.
    //
    // CHECK: ==> Enter 'testDifferentTasksHaveDifferentExclusivityAccessSets'
    //
    // This is our the access from await callee1(&global1). We should have only
    // one access.
    // CHECK-NEXT: Inserting new access: [[LINKEDLIST_NODE1:0x[0-9a-f]+]]
    // CHECK-NEXT: Tracking
    // CHECK-NEXT:   Access. Pointer: [[ACCESS1_PTR:0x[0-9a-f]+]]
    //
    // CHECK-NEXT: ==> Enter callee1
    //
    // Now we are inside callee1 and we hit our first suspend point to call
    // callee2. So we need to swizzle the memory. We want to check that our
    // thread local storage after we are done swizzling has as its head/tail our
    // single access and that the exclusivity runtime doesn't see any live
    // accesses. Our initial task state should be initialized to 0.
    //
    // CHECK-NEXT: Exiting Thread Local Context. Before Swizzle.
    // CHECK-NEXT:    SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): (0x0, 0x0)
    // CHECK-NEXT:    Access. Pointer: [[ACCESS1_PTR]]. PC: {{0x[0-9a-f]+}}. AccessAction: modification
    // CHECK-NEXT: Exiting Thread Local Context. After Swizzle.
    // CHECK-NEXT:    SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): ([[LINKEDLIST_NODE1]], [[LINKEDLIST_NODE1]])
    // CHECK-NEXT:    No Accesses.
    //
    // There are a bunch of other pushing/popping of the null case. We are
    // uninterested beyond making sure there are no more accesses.
    // CHECK-NOT: Inserting new access:
    //
    // When we enter the callee1 closure, we are going to insert the conflicting
    // access. It is from a different thread and we properly swizzled so the
    // exclusivity runtime will not see anything. There are no await statements
    // so no swizzling happens until we return fraom callee1 closure.
    //
    // CHECK: ==> Enter callee1 closure
    // CHECK-NEXT: Inserting new access: [[LINKEDLIST_NODE2:0x[a-z0-9]+]]
    // CHECK-NEXT: Tracking!
    // CHECK-NEXT:     Access. Pointer: [[ACCESS2_PTR:0x[a-z0-9]+]].
    // CHECK-NEXT: ==> Enter callee2
    // CHECK-NEXT: ==> Exit callee2
    // CHECK-NEXT: Removing access: [[LINKEDLIST_NODE2]]
    // CHECK-NEXT: ==> Exit callee1 closure
    //
    // Now that we have exited from callee1 closure, we swizzle back in our
    // original access list from the task.
    //
    // CHECK-NEXT: Entering Thread Local Context. Before Swizzle.
    // CHECK-NEXT:     SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): ([[LINKEDLIST_NODE1]], [[LINKEDLIST_NODE1]])
    // CHECK-NEXT:     No Accesses.
    // CHECK-NEXT: Entering Thread Local Context. After Swizzle.
    // CHECK-NEXT:     SwiftTaskThreadLocalContext: (FirstAccess,LastAccess): (0x0, 0x0)
    // CHECK-NEXT:     Access. Pointer: [[ACCESS1_PTR]].
    //
    // We then do a bunch of switching back and forth due to unoptimized
    // hopping. Just make sure we do not insert any more accesses.
    //
    // CHECK-NOT: Inserting new access:
    //
    // CHECK: ==> Exit callee1
    // CHECK-NEXT: Removing access: [[LINKEDLIST_NODE1]]
    //
    // CHECK: ==> Exit 'testDifferentTasksHaveDifferentExclusivityAccessSets'
    @MainActor static func testDifferentTasksHaveDifferentExclusivityAccessSets() async {
        let callee2 = { @MainActor (_ x: inout Int) -> Void in
            print("==> Enter callee2")
            print("==> Exit callee2")
        }

        // We add an inline never here to make sure that we do not eliminate
        // the dynamic access after inlining.
        @MainActor
        @inline(never)
        func callee1(_ x: inout Int) async -> () {
            print("==> Enter callee1")
            // This task is what prevents this example from crashing.
            let handle = Task { @MainActor in
                print("==> Enter callee1 closure")
                // Second access. Different Task so it is ok.
                callee2(&global1)
                print("==> Exit callee1 closure")
            }
            await handle.value
            print("==> Exit callee1")
        }

        print("==> Enter 'testDifferentTasksHaveDifferentExclusivityAccessSets'")
        // First access begins here.
        await callee1(&global1)
        print("==> Exit 'testDifferentTasksHaveDifferentExclusivityAccessSets'")
    }

    @MainActor static func main() async {
        await testDifferentTasksHaveDifferentExclusivityAccessSets()
    }
}
