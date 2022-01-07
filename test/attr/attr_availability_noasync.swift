// RUN: %target-typecheck-verify-swift

// REQUIRES: concurrency


@available(*, noasync)
func basicNoAsync() { }

@available(*, noasync, message: "a message from the author")
func messageNoAsync() { }

@available(*, noasync, renamed: "asyncReplacement()")
func renamedNoAsync(_ completion: @escaping (Int) -> Void) -> Void { }

@available(macOS 11, *)
func asyncReplacement() async -> Int { }

@available(*, noasync, renamed: "IOActor.readString()")
func readStringFromIO() -> String {}

@available(macOS 11, *)
actor IOActor {
    func readString() -> String {
        return readStringFromIO()
    }
}

@available(macOS 11, *)
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
