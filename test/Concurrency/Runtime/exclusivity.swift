// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// This test makes sure that:
//
// 1. Tasks have separate exclusivity sets.
// 2. Exercise the pushing/popping of access sets from tasks.

// NOTE: The cases that we are talking about handling below refer to the cases
// documented in Exclusivity.cpp.
//
// NOTE: We test cases that involve custom executors in
// custom_executors_exclusivity.cpp.

import _Concurrency
import StdlibUnittest

var global1: Int = 5
var global2: Int = 6
var global3: Int = 7

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#elseif canImport(CRT)
import CRT
#endif

@inlinable
public func debugLog(_ s: String) {
    // Only enable this when debugging test failures against an asserts
    // runtime. Otherwise, the test is noisy and has non-windows
    // dependencies. We need stderr to ensure proper log output interleaving
    // with the runtime's own stderr emitted output.
#if DEBUG_LOGGING
    fputs(s + "\n", stderr)
    fflush(stderr)
#endif
}

@available(SwiftStdlib 5.5, *)
@main
struct Runner {
    @inline(never)
    @MainActor
    static func doSomething() async { }

    @inline(never)
    @Sendable
    static func useGlobal(_ x: inout Int) { debugLog("FORCE ACCESS") }

    @MainActor static func main() async {
        var exclusivityTests = TestSuite("Async Exclusivity")

        // First make sure that if we do not introduce a new task, we still get
        // our expected conflict.
        exclusivityTests.test("testSameTaskBlowsUpSinceSameSet") { @MainActor in
            expectCrashLater(withMessage: "Fatal access conflict detected")

            let callee2 = { @MainActor (_ x: inout Int) -> Void in
                debugLog("==> Enter callee2")
                debugLog("Value: x: \(x)")
                debugLog("==> Exit callee2")
            }

            // We add an inline never here to make sure that we do not eliminate
            // the dynamic access after inlining.
            @MainActor
            @inline(never)
            func callee1(_ x: inout Int) async -> () {
                debugLog("==> Enter callee1")

                // Second access. Same Task so not ok.
                await callee2(&global1)

                debugLog("==> Exit callee1")

            }

            debugLog("==> Enter Main")
            // First access begins here.
            await callee1(&global1)
            debugLog("==> Exit Main")
        }

        // Then do a simple test with a single access to make sure that we do
        // not hit any sccesses b/c we introduced the Task.
        exclusivityTests.test("testDifferentTasksHaveDifferentExclusivityAccessSets") { @MainActor in
            let callee2 = { @MainActor (_ x: inout Int) -> Void in
                debugLog("==> Enter callee2")
                debugLog("==> Exit callee2")
            }

            // We add an inline never here to make sure that we do not eliminate
            // the dynamic access after inlining.
            @MainActor
            @inline(never)
            func callee1(_ x: inout Int) async -> () {
                debugLog("==> Enter callee1")
                // This task is what prevents this example from crashing.
                let handle = Task { @MainActor in
                    debugLog("==> Enter callee1 Closure")
                    // Second access. Different Task so it is ok.
                    await callee2(&global1)
                    debugLog("==> Exit callee1 Closure")
                }
                await handle.value
                debugLog("==> Exit callee1")
            }

            debugLog("==> Enter Main")
            // First access begins here.
            await callee1(&global1)
            debugLog("==> Exit Main")
        }

        // Make sure we correctly handle cases where we have multiple accesses
        // open at the same time.
        exclusivityTests.test("testDifferentTasksWith2Accesses") { @MainActor in
            let callee2 = { @MainActor (_ x: inout Int, _ y: inout Int) -> Void in
                debugLog("==> Enter callee2")
                debugLog("==> Exit callee2")
            }

            // We add an inline never here to make sure that we do not eliminate
            // the dynamic access after inlining.
            @MainActor
            @inline(never)
            func callee1(_ x: inout Int, _ y: inout Int) async -> () {
                debugLog("==> Enter callee1")
                let handle = Task { @MainActor in
                    debugLog("==> Enter callee1 Closure")
                    // Second access. Different Task so it is ok.
                    await callee2(&global1, &global2)
                    debugLog("==> Exit callee1 Closure")
                }
                await handle.value
                debugLog("==> Exit callee1")
            }

            debugLog("==> Enter Main")
            // First access begins here.
            await callee1(&global1, &global2)
            debugLog("==> Exit Main")
        }

        // Make sure we correctly handle cases where we have multiple accesses
        // open at the same time.
        exclusivityTests.test("testDifferentTasksWith3Accesses") { @MainActor in
            let callee2 = { @MainActor (_ x: inout Int, _ y: inout Int, _ z: inout Int) -> Void in
                debugLog("==> Enter callee2")
                debugLog("==> Exit callee2")
            }

            // We add an inline never here to make sure that we do not eliminate
            // the dynamic access after inlining.
            @MainActor
            @inline(never)
            func callee1(_ x: inout Int, _ y: inout Int, _ z: inout Int) async -> () {
                debugLog("==> Enter callee1")
                let handle = Task { @MainActor in
                    debugLog("==> Enter callee1 Closure")
                    // Second access. Different Task so it is ok.
                    await callee2(&global1, &global2, &global3)
                    debugLog("==> Exit callee1 Closure")
                }
                await handle.value
                debugLog("==> Exit callee1")
            }

            debugLog("==> Enter Main")
            // First access begins here.
            await callee1(&global1, &global2, &global3)
            debugLog("==> Exit Main")
        }

        // Now that we have tested our tests with various numbers of accesses,
        // lets make specific tests for each case in Exclusivity.cpp.
        //
        // Case 1: (F, F, F) - No Live Accesses at Task Start, No Live Sync
        // Accesses When Push, No Live Task Accesses when pop.
        //
        // This case is the case where we do not have any accesses in our code
        // at all or if the task cleans up the tasks before it awaits again. We
        // test the task cleanup case.
        exclusivityTests.test("case1") { @MainActor in
            @inline(never)
            @Sendable func callee2(_ x: inout Int, _ y: inout Int, _ z: inout Int) -> Void {
                debugLog("==> Enter callee2")
                debugLog("==> Exit callee2")
            }

            // We add an inline never here to make sure that we do not eliminate
            // the dynamic access after inlining.
            @MainActor
            @inline(never)
            func callee1() async -> () {
                debugLog("==> Enter callee1")
                let handle = Task { @MainActor in
                    debugLog("==> Enter callee1 Closure")
                    // These accesses end before we await in the task.
                    do {
                        callee2(&global1, &global2, &global3)
                    }
                    await doSomething()
                    debugLog("==> Exit callee1 Closure")
                }
                await handle.value
                debugLog("==> Exit callee1")
            }

            await callee1()
        }

        // In case 2, our task does not start with any live accesses, but it is
        // awaited upon after the live access begins. We want to make sure that
        // we fail here since we properly restored the callee state.
        exclusivityTests.test("case2") { @MainActor in
            expectCrashLater(withMessage: "Fatal access conflict detected")

            let callee2 = { @MainActor (_ x: inout Int) -> Void in
                debugLog("==> Enter callee2")
                debugLog("==> Exit callee2")
            }

            // We add an inline never here to make sure that we do not eliminate
            // the dynamic access after inlining.
            @MainActor
            @inline(never)
            func callee1(_ x: inout Int) async -> () {
                debugLog("==> Enter callee1")
                // This task is what prevents this example from crashing.
                let handle = Task { @MainActor in
                    debugLog("==> Enter callee1 Closure")
                    // Second access. Different Task so it is ok.
                    await callee2(&global1)
                    debugLog("==> Exit callee1 Closure")
                }
                await handle.value

                useGlobal(&global1) // We should crash here.

                debugLog("==> Exit callee1")
            }

            debugLog("==> Enter Main")
            // First access begins here.
            await callee1(&global1)
            debugLog("==> Exit Main")
        }

        // In case 5, our task starts with live accesses, but we finish the
        // accesses before we return. So we do not crash. The key thing is the
        // access lives over a suspension/resume.
        exclusivityTests.test("case5") { @MainActor in
            let callee2 = { @MainActor (_ x: inout Int) -> Void in
                debugLog("==> Enter callee2")
                debugLog("==> Exit callee2")
            }

            // We add an inline never here to make sure that we do not eliminate
            // the dynamic access after inlining.
            @MainActor
            @inline(never)
            func callee1(_ x: inout Int) async -> () {
                debugLog("==> Enter callee1")
                // This task is what prevents this example from crashing.
                let handle = Task { @MainActor in
                    debugLog("==> Enter callee1 Closure")
                    // Second access. Different Task so it is ok.
                    await callee2(&global1)
                    debugLog("==> Exit callee1 Closure")
                }
                await handle.value

                debugLog("==> Exit callee1")
            }

            @MainActor
            @inline(never)
            func callCallee1() async {
                await callee1(&global1)
            }

            debugLog("==> Enter Main")
            // First access begins here.
            await callCallee1()
            useGlobal(&global1) // We should not crash here since we cleaned up
                                // the access in callCallee1 after we returned
                                // from the await there.
            debugLog("==> Exit Main")
        }

        // In case 6, our task starts with live accesses, and we only finish
        // some of the accesses before we return. So we want to validate that by
        // running the same code twice, one testing we can access the pointer we
        // can fix up and a second that we can not.
        exclusivityTests.test("case6") { @MainActor in
            let callee2 = { @MainActor (_ x: inout Int) -> Void in
                debugLog("==> Enter callee2")
                debugLog("==> Exit callee2")
            }

            // We add an inline never here to make sure that we do not eliminate
            // the dynamic access after inlining.
            @MainActor
            @inline(never)
            func callee1(_ x: inout Int) async -> () {
                debugLog("==> Enter callee1")
                // This task is what prevents this example from crashing.
                let handle = Task { @MainActor in
                    debugLog("==> Enter callee1 Closure")
                    // Second access. Different Task so it is ok.
                    await callee2(&global1)
                    debugLog("==> Exit callee1 Closure")
                }
                await handle.value

                debugLog("==> Exit callee1")
            }

            @MainActor
            @inline(never)
            func callCallee1() async {
                await callee1(&global1)
            }

            debugLog("==> Enter Main")
            // First access begins here.
            await callCallee1()
            useGlobal(&global1) // We should not crash here since we cleaned up
                                // the access in callCallee1 after we returned
                                // from the await there.
            debugLog("==> Exit Main")
        }

        await runAllTestsAsync()
    }
}
