# Distributed

Build systems that run distributed code across multiple processes and devices.

Distributed actors share many characteristics with Swift actors,
and include additional isolation checks to ensure
location transparency and safety in a distributed environment.
Similar to how actors make it easier to write concurrent code
that's safe and correct to run on a single computer,
distributed actors make it easier to write code
that runs across multiple computers.

![A diagram showing two columns of actors. The left column includes a remote actor reference. The right column includes a local distributed actor. An arrow points from the remote actor reference to the local distributed actor that it refers to.](distributed-module)

You use three main parts when writing code with distributed actors:

- Swift language support for distributed actors and for building distributed or RPC systems integrated into the language.
  For more information,
  see [Concurrency][concurrency] in [The Swift Programming Language][tspl].

- The Distributed module, which includes the types and protocols you need
  to declare and use distributed actors.
  For example, it has
  protocols to which distributed actors and distributed actor systems conform,
  and structures that encapsulate information about calls to a distributed actor.

- A *distributed actor system* provides an implementation of the
  ``Distributed/DistributedActorSystem`` protocol
  and coordinates between the cluster's nodes.
  A distributed actor is always part of some distributed actor system;
  that distributed actor system handles the serialization and networking
  necessary to perform remote method calls.
  For local testing, you can use ``Distributed/LocalTestingDistributedActorSystem``.
  For production,
  you can use `ClusterSystem` offered by
  the [Swift Distributed Actors][cluster] library,
  use another library,
  or [write your own distributed actor system](<doc:implementing-a-custom-distributed-actor-system>).

## Oneway Methods

By default, every remote call made on a distributed actor follows a
request/response pattern: the caller `await`s a reply from the peer, and the
actor system's `remoteCall` / `remoteCallVoid` implementation is responsible
for shipping the invocation over the network, awaiting the peer's response,
and delivering the returned value or thrown error back to the caller.

Some transports (particularly classical actor-model style messaging) benefit
from opting out of the response half of that pattern. The trailing `oneway`
function modifier marks a method as fire-and-forget. Calls to it must be
spelled with the `nowait` keyword, which does not wait for the call to run.
For remote calls the compiler also carries the modifier as a *hint* through the
synthesized thunk into ``Distributed/RemoteCallTarget``, so the actor system
can decide whether to take an alternative code path. Individual actor systems
are free to ignore the hint entirely.

`oneway` is experimental and gated by
`-enable-experimental-feature OnewayNowait`.

### The `oneway` modifier

A oneway remote call is fire-and-forget: the actor system may complete the
local side of the call as soon as the outgoing message has been written, and
should not wait for a peer reply. This matches the "message" concept of
classical actor systems, and is well suited to keep-alive messages, batched
acknowledgements, and other traffic where a response would only be discarded.

`oneway` is spelled as a trailing function modifier, after the effect
specifiers:

```swift
distributed func ping() oneway
distributed func log(_ line: String) async oneway
```

A `oneway` function may be `async`, but it must not be `throws`: nobody
observes its result, so there is nobody to receive an error either. The
synthesized thunk still invokes `try await remoteCallVoid(...)`, so the actor
system is allowed to suspend the sending task until the outbound write
completes and to throw on send failure. The contract is only that it must not
depend on a reply from the peer.

Only `Void`-returning functions may carry the `oneway` modifier; the compiler
rejects a non-`Void` result at the declaration site. `oneway` is accepted on
`distributed` functions, on actor instance methods and on global-actor-isolated
functions, and a call to any of them must be spelled `nowait`:

```swift
nowait greeter.ping()
```

The actor system observes the hint via
``Distributed/RemoteCallTarget/isOnewayRemoteCall`` inside its
`remoteCallVoid` implementation.

Outside of Embedded Swift a `oneway` distributed function requires the
availability of the Swift 6.5 runtime, because its remote call target
identifier carries the `oneway` marker, which a recipient running an older
runtime cannot demangle. Plain actor and global-actor-isolated `oneway`
methods have no remote call target, and have no such requirement.

### Synchronous oneway calls in Embedded Swift

In Embedded Swift, a `oneway` function whose body is synchronous is lowered
without any async code on the sending side, so its callers and its distributed
thunk have no suspension points:

```swift
distributed actor Greeter {
  distributed func greet(_ name: String) oneway { ... }
}

func send(to greeter: Greeter) throws {
  try nowait greeter.greet("Caplin") // callable from a synchronous function
}
```

- The distributed thunk is `throws` but not `async`, so the call is spelled
  `try nowait`.
- For a remote actor the thunk calls the synchronous
  `remoteCallVoidOneway(on:target:invocation:throwing:)` requirement without
  `await`, and ``Distributed/RemoteCallTarget/isOnewayRemoteCall`` is set. An
  actor system that doesn't implement it gets a default implementation, which
  runs `remoteCallVoid` from a `Task.immediate` and drops any remote error.
- For a local actor the call is enqueued on the actor: a discarding task that
  copies the caller's task locals starts on the actor's executor and calls
  the method. Calls enqueued on the same actor run in the order they were
  made.
- The synthesized receive dispatcher decodes the arguments and enqueues the
  call the same way, and sends no reply.

The same lowering applies to synchronous `oneway` methods of plain actors and
of global actors: `nowait a.f()` enqueues the call on the actor without a
`try`, and can be written in a synchronous function.

`oneway` functions with an `async` body, and all `oneway` functions outside
Embedded Swift, keep the task-based `nowait` lowering and the `try await
remoteCallVoid(...)` thunk described above.

[concurrency]: https://docs.swift.org/swift-book/LanguageGuide/Concurrency.html
[tspl]: https://docs.swift.org/swift-book/
[cluster]: https://github.com/apple/swift-distributed-actors/

## Topics

### Distributed actors

- ``Distributed/DistributedActor``
- ``Distributed/Resolvable()``
- ``Distributed/buildDefaultDistributedRemoteActorExecutor(_:)``

### Distributed actor system

- <doc:implementing-a-custom-distributed-actor-system>
- ``Distributed/DistributedActorSystem``
- ``Distributed/RemoteCallTarget``
- ``Distributed/RemoteCallTarget/isOnewayRemoteCall``
- ``Distributed/RemoteCallArgument``
- ``Distributed/DistributedTargetInvocationEncoder``
- ``Distributed/DistributedTargetInvocationDecoder``
- ``Distributed/DistributedTargetInvocationResultHandler``


### Local testing

- ``Distributed/LocalTestingDistributedActorSystem``
- ``Distributed/LocalTestingActorID``
- ``Distributed/LocalTestingActorAddress``
- ``Distributed/LocalTestingInvocationEncoder``
- ``Distributed/LocalTestingInvocationDecoder``
- ``Distributed/LocalTestingInvocationResultHandler``

### Errors

- ``Distributed/DistributedActorCodingError``
- ``Distributed/DistributedActorSystemError``
- ``Distributed/ExecuteDistributedTargetError``
- ``Distributed/LocalTestingDistributedActorSystemError``
