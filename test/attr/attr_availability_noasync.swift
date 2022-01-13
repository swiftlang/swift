// RUN: %target-typecheck-verify-swift

// REQUIRES: concurrency


@available(*, noasync)
func basicNoAsync() { }

@available(*, noasync, message: "a message from the author")
func messageNoAsync() { }

@available(*, noasync, renamed: "asyncReplacement()")
func renamedNoAsync(_ completion: @escaping (Int) -> Void) -> Void { }

@available(macOS 11, iOS 13, watchOS 6, *)
func asyncReplacement() async -> Int { }

@available(*, noasync, renamed: "IOActor.readString()")
func readStringFromIO() -> String {}

@available(macOS 11, iOS 13, watchOS 6, *)
actor IOActor {
    func readString() -> String {
        return readStringFromIO()
    }
}

@available(macOS 11, iOS 13, watchOS 6, *)
func asyncFunc() async {
    // expected-error@+1{{global function 'basicNoAsync' is unavailable from asynchronous contexts}}
    basicNoAsync()

    // expected-error@+1{{global function 'messageNoAsync' is unavailable from asynchronous contexts; a message from the author}}
    messageNoAsync()

    // expected-error@+1{{global function 'renamedNoAsync' is unavailable from asynchronous contexts}}{{5-19=asyncReplacement}}
    renamedNoAsync() { _ in }

    // expected-error@+1{{global function 'readStringFromIO' is unavailable from asynchronous contexts}}{{13-29=IOActor.readString}}
    let _ = readStringFromIO()
}

// expected-error@+3{{asynchronous global function 'unavailableAsyncFunc()' must be available from asynchronous contexts}}
@available(macOS 11, iOS 13, watchOS 6, *)
@available(*, noasync)
func unavailableAsyncFunc() async {
}

@available(macOS 11, iOS 13, watchOS 6, *)
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
