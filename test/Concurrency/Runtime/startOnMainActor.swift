// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN:  %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

import StdlibUnittest
@_spi(MainActorUtilities) import _Concurrency

func doStuffAsync() async {
    await Task.sleep(500)
}

let tests = TestSuite("StartOnMainActor")

tests.test("startOnMainActor") {
    // "global" variables for this test
    struct Globals {
        @MainActor
        static var ran = false
    }

    @MainActor
    func run() async {
        Globals.ran = true
        await doStuffAsync()
    }

    // enqueue item on the MainActor
    let t1 = Task { @MainActor in
        await Task.sleep(1000)
    }

    expectFalse(Globals.ran)

    // Run something with side-effects on the main actor
    let t2 = Task.startOnMainActor {
        return await run()
    }

    expectTrue(Globals.ran)
    await t1.value
    await t2.value
}

tests.test("throwing startOnMainActor") {
    // "global" variables for this test
    struct Globals {
        @MainActor
        static var ran = false
    }

    struct StringError: Error {
        let message: String
    }

    @MainActor
    func run() async throws {
        Globals.ran = true
        await doStuffAsync()
        throw StringError(message: "kablamo!")
    }

    // enqueue item on the MainActor
    let t1 = Task { @MainActor in
        await Task.sleep(1000)
    }

    expectFalse(Globals.ran)

    // Run something with side-effects on the main actor
    let t2 = Task.startOnMainActor {
        return try await run()
    }

    expectTrue(Globals.ran)
    await t1.value
    expectNil(try? await t2.value)
}

await runAllTestsAsync()
