// RUN: %target-typecheck-verify-swift

// REQUIRES: concurrency


@available(*, noasync)
func basicNoAsync() { }

@available(*, noasync, message: "a message from the author")
func messageNoAsync() { }

@available(*, noasync, renamed: "asyncReplacement()")
func renamedNoAsync(_ completion: @escaping (Int) -> Void) -> Void { }

@available(SwiftStdlib 5.5, *)
func asyncReplacement() async -> Int { }

@available(*, noasync, renamed: "IOActor.readString()")
func readStringFromIO() -> String {}

@available(SwiftStdlib 5.5, *)
actor IOActor {
    func readString() -> String {
        return readStringFromIO()
    }
}

@available(SwiftStdlib 5.5, *)
func asyncFunc() async {
    // expected-warning@+1{{global function 'basicNoAsync' is unavailable from asynchronous contexts; this is an error in Swift 6}}
    basicNoAsync()

    // expected-warning@+1{{global function 'messageNoAsync' is unavailable from asynchronous contexts; a message from the author}}
    messageNoAsync()

    // expected-warning@+1{{global function 'renamedNoAsync' is unavailable from asynchronous contexts}}{{5-19=asyncReplacement}}
    renamedNoAsync() { _ in }

    // expected-warning@+1{{global function 'readStringFromIO' is unavailable from asynchronous contexts}}{{13-29=IOActor.readString}}
    let _ = readStringFromIO()
}

// expected-error@+3{{asynchronous global function 'unavailableAsyncFunc()' must be available from asynchronous contexts}}
@available(SwiftStdlib 5.5, *)
@available(*, noasync)
func unavailableAsyncFunc() async {
}

@available(SwiftStdlib 5.5, *)
protocol BadSyncable {
    // expected-error@+2{{asynchronous property 'isSyncd' must be available from asynchronous contexts}}
    @available(*, noasync)
    var isSyncd: Bool { get async }

    // expected-error@+2{{asynchronous instance method 'sync' must be available from asynchronous contexts}}
    @available(*, noasync)
    func sync(_ str: String) async
}

class TestClass {
    // expected-error@+2{{'@available' attribute cannot be applied to this declaration}}
    @available(*, noasync)
    deinit { }
}
