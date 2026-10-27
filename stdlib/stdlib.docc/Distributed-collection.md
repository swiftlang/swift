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
from opting out of the response half of that pattern. The `@oneway`
attribute marks a method as fire-and-forget. Calls to it must be spelled with
the `send` keyword, which does not wait for the call to run.

`@oneway` and `send` are experimental and gated by
`-enable-experimental-feature SendCalls`.

### The `@oneway` attribute

A oneway call is fire-and-forget: it matches the "message" concept of
classical actor systems, and is well suited to keep-alive messages, batched
acknowledgements, and other traffic where a response would only be discarded.

`@oneway` is an attribute of the function declaration:

```swift
@oneway distributed func ping()
@oneway distributed func log(_ line: String) async
```

A `@oneway` function may be `async`, but it must not throw: nobody observes
its result, so there is nobody to receive an error either. Only
`Void`-returning functions may be `@oneway`. A `@oneway` function must be
isolated to an actor, which makes it either a `distributed` function or an
actor method. A `@oneway` function is meant to be called with `send`, and an
`await` of one is a warning, see the `AwaitedOnewayCall` diagnostic group.

`send` itself can be used to call any actor isolated function which returns
`Void` and does not throw, without waiting for it, whether it is `@oneway` or
not. Calling a function which is not actor isolated with `send` is an error.
`send` cannot be combined with `await`, in either order: `await send f()` and
`send await f()` are errors, because `send` does not wait for the call.

### Oneway distributed functions

A `@oneway` distributed function can only be declared in a distributed actor
whose actor system is known to conform to
`_DistributedOnewayActorSystem`: a concrete actor system type
which conforms to it, or a generic actor system which is constrained to
conform to it, for example in a `@Resolvable` protocol:

```swift
@Resolvable
protocol Worker: DistributedActor
  where ActorSystem: _DistributedOnewayActorSystem<any Codable> {
  @oneway distributed func stop()
}
```

The actor system side of the feature is SPI in the `OnewaySend`
group: `_DistributedOnewayActorSystem` and its `remoteCallOneway` requirement,
and the `isOnewayRemoteCall` and `isOnewayTarget` properties of
`RemoteCallTarget`. Implementing an actor system which supports `@oneway`
functions, naming the protocol in a constraint as above, or reading the
oneway flags of a target needs
`@_spi(OnewaySend) import Distributed`, and a conformance of a
public type to the protocol must be declared `@_spi(OnewaySend)`
as well. Declaring a `@oneway` function, and calling one or any other
function with `send`, needs no SPI import.

The distributed thunk of a `@oneway` function, `async` or not, is synchronous:
it is `throws` but not `async`, so the call is spelled `try send` and can be
made from a synchronous function:

```swift
func stop(_ worker: some Worker) throws {
  try send worker.stop()
}
```

- For a remote actor the thunk calls the synchronous
  `remoteCallOneway(on:target:invocation:)` of the actor system without
  `await`, and `RemoteCallTarget.isOnewayRemoteCall` is set. It has no
  `throwing:` parameter because a `@oneway` function cannot throw, and it can
  throw if the call cannot be handed to the transport. It must not wait for a
  reply from the peer.
- For a local actor the call runs on the actor without the caller waiting for
  it. Calls made on the same actor run in the order they were made.
- On the receiving side the call is executed like any other distributed call.

Outside of Embedded Swift a `@oneway` distributed function requires the
availability of the Swift 6.5 runtime, because its remote call target
identifier carries the `@oneway` marker, which a recipient running an older
runtime cannot demangle. Other `@oneway` functions have no remote call
target, and have no such requirement.

A `send` of a distributed function which is not `@oneway` is an error when
the call may be remote, since only the synchronous thunk of a `@oneway`
function can hand the call to the actor system as a oneway call: mark the
function `@oneway`, or call it with `try await` to wait for its reply. A call
on a known-local actor, such as an isolated `self`, does not go through a
thunk and is sent like any actor call.

An `await` of a `@oneway` function is a warning, see the `AwaitedOnewayCall`
diagnostic group.

### Protocol requirements and witnesses

A `@oneway` requirement of a protocol can be witnessed by a function that is
not `@oneway`, including a `distributed` one: such a function has fewer
obligations, as it can also be called without `send`, and a call of the
requirement through the protocol, which is made by `send`, only needs it to
not wait. The witness is called through an adapter thunk, which behaves like
the thunk of a `@oneway` function.

The reverse does not work: a `@oneway` function cannot witness a requirement
that is not `@oneway`, as the callers of the requirement do not call it with
`send`.

The stub that `@Resolvable` generates for a `@oneway` requirement keeps the
`@oneway` attribute, and like the stub of any other requirement it never runs
the function locally.

### Synchronous oneway calls in Embedded Swift

In Embedded Swift, a call of a synchronous `@oneway` function on a local actor
is enqueued on the actor without any async code: a discarding task that copies
the caller's task locals starts on the actor's executor and calls the method.
The synthesized receive dispatcher decodes the arguments and enqueues the call
the same way, and sends no reply.

The same lowering applies to a `send` of any synchronous method of a plain
actor and of any synchronous function isolated to a global actor, `@oneway`
or not: `send a.f()` enqueues the call on the actor without a `try`, and can
be written in a synchronous function.

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
