# Awaiting a `@oneway` function (AwaitedOnewayCall)

## Overview

A function declared `@oneway` is designed to be invoked with the `send` keyword, without awaiting for the function to return.

It is possible to call any Void-returning, non-throwing, isolated function with `send`.
If a method is explicitly annotated using `@oneway` it is inherently designed to be used in an oneway or "fire-and-forget" style, and awaiting it could cause unnecessary synchronization.

Calling a function with `send` enqueues the call on the function's actor, and executes the target method without waiting for it to complete.
A method annotated with `@oneway` explicitly signals that it was designed with this in mind, and calling it with `await` may not result in optimal behavior.

The warning in this group is emitted when a `@oneway` function is called with `await` from outside of its isolation domain. The call is made as an ordinary call, which works, but does not express the intent of the function. The `await` waits for the function to run, like for any other awaited call.

For example:

```swift
actor Service {
  @oneway func enqueue(_ message: String) {
    // ...
  }

  func startProcessing() {
    enqueue("work") // OK, a call from within the actor runs in place, like any other call
  }
}

func process(service: Service) async {
  await service.enqueue("work") // warning: '@oneway' instance method 'enqueue' is not intended to be awaited on, use 'send' instead
  send service.enqueue("work")  // OK

  send service.startProcessing() // not marked @oneway; calling it with either 'send' or 'await' is permitted
}
```

A call from within the function's own isolation domain, such as `enqueue("work")` from inside the actor, does not wait for another actor, and is not diagnosed. It can still be made with `send`, which enqueues the call on the actor itself, to run after the current job has completed.

Note also that it is possible to call isolated methods from a synchronous context, where `await` would not be possible to use, with `send` instead:

```swift
func synchronous(service: Service) {
  await service.enqueue("work") // error: cannot use 'await' in synchronous context

  send service.enqueue("work") // OK
}
```

### Sending calls retains enqueue ordering

Prefer using `send` instead of creating a `Task` to service such one off calls, in order to retain the enqueue order of calls:

```swift
actor Printer {
  func print(_ message: String) { Swift.print(message) }
}

func test(printer: Printer) {
  Task {
    await printer.print("A")
  }
  Task {
    await printer.print("B")
  }
  // both print orderings:
  //   A
  //   B
  // or
  //   B
  //   A
  // might appear, and are legal.
}

func testSend(printer: Printer) {
  send printer.print("A")
  send printer.print("B")
  // Guaranteed ordering (unless a custom executor reorders):
  //   A
  //   B
}
```

### Controlling the warning

Calling a `@oneway` method with `await` is not an error by default, as it is only ignoring a design hint from the target method's author,
and usually may only result in a less efficient program, but rarely would result in an incorrect execution.

The warning may be silenced for a declaration, including everything inside it, using the `@diagnose` attribute:

```swift
@diagnose(AwaitedOnewayCall, as: ignored, reason: "waits for the message to be enqueued on purpose")
func flush(service: Service) async {
  await service.enqueue("flush") // no warning
}
```

To silence it for every declaration in a file, write `default @diagnose(AwaitedOnewayCall, as: ignored)` at the top of the file.

The warning can be turned into an error with the `-Werror AwaitedOnewayCall` compiler flag for a whole module, or with `@diagnose(AwaitedOnewayCall, as: error)` for a single declaration.
There is no compiler flag that silences the warning for a whole module.

A call which crosses an actor isolation boundary without `send` or `await` is always an error, which is not part of this group. Silencing the group with `@diagnose(AwaitedOnewayCall, as: ignored)` only suppresses the warning about `await`, and not these errors.
