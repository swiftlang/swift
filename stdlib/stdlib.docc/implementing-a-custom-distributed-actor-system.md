# Implementing a Custom Distributed Actor System

Implement a `DistributedActorSystem` to provide a custom transport layer
for distributed actors.

## Overview

A ``Distributed/DistributedActor`` is always associated with a *distributed actor system*,
which determines how `distributed func` calls on a *remote* actor are executed.

To declare a distributed actor, use the `distributed actor` keywords and choose
the actor system it belongs to. The actor system provides the "transport" —
such as a network or interprocess channel — to an actor, regardless of where that actor runs.

Code that *uses* distributed actors doesn't need to interact with this protocol directly —
that abstraction is the point. Implement a `DistributedActorSystem` when you're
providing a new transport implementation, such as a cluster, a WebSocket client or server,
or an interprocess communication (IPC) system.

> Tip: In other words, ``Distributed/DistributedActorSystem`` provides the expected interface for your
> own RPC frameworks that integrate with the Swift runtime and concurrency model.

An actor system manages the lifecycle and remote interactions of distributed actors.
These responsibilities map to specific methods on the `DistributedActorSystem` protocol:

- **Define** the actor system and its associated types.
- **Assign**, track, and release actor identities.
- **Resolve** an actor identity to either a local instance or a remote reference.
- **Call** a remote method on a distributed actor.
- **Encode** an outgoing invocation, send it to the remote peer, and await a reply.
- **Decode** an incoming invocation and dispatch it to the target actor.
- **Reply** with the results and errors.

The rest of this article walks through each responsibility, providing examples
that build to a minimal in-memory transport. For deeper coverage of the language
and runtime semantics, see [SE-0336: Distributed Actor Isolation][SE-0336] and
[SE-0344: Distributed Actor Runtime][SE-0344].

[SE-0336]: https://github.com/apple/swift-evolution/blob/main/proposals/0336-distributed-actor-isolation.md
[SE-0344]: https://github.com/apple/swift-evolution/blob/main/proposals/0344-distributed-actor-runtime.md

### Define the actor system and associated types

A `DistributedActorSystem` is typically a final class — or a struct that wraps a class —
because it holds internal state such as the mapping from identifiers to actors, and other
components reference it by identity.

To implement a distributed actor system, declare a new type that conforms to the
``Distributed/DistributedActorSystem`` protocol. Then provide witnesses for the five
required associated types, shown below and explored in the sections that follow.

```swift
import Distributed

public final class SampleActorSystem: DistributedActorSystem {
    public typealias ActorID = SampleActorID
    public typealias SerializationRequirement = any Codable
    public typealias InvocationEncoder = SampleInvocationEncoder
    public typealias InvocationDecoder = SampleInvocationDecoder
    public typealias ResultHandler = SampleResultHandler

    // Internal state filled in below.
}
```

### Assign, track, and release actor identities

An `ActorID` identifies a distributed actor from the moment it's created. The actor system
serializes and sends this identifier to remote peers during network calls.
This identifier lets you send "references" to an actor to other nodes or processes; the
recipient calls ``Distributed/DistributedActor/resolve(id:using:)`` on the identifier to
obtain a *remote reference* to the actor.

```swift
public struct SampleActorID: Hashable, Sendable, Codable {
    public let node: String
    public let instance: UUID
}
```

Conforming `SampleActorID` to `Codable` makes it compatible with the actor system's
`SerializationRequirement`.

Because ``Distributed/DistributedActor`` is `Codable` whenever its `ID` is, choosing a
`Codable` `ActorID` makes distributed actors serializable as arguments to other distributed
calls. For example, you can make a remote call like this:

```swift
distributed actor Worker {
  distributed func introduce(another: Worker) { ... }
}

let remoteWorker: Worker = ... // A worker located in a different process.
let charlie: Worker = Worker(actorSystem: ...) // A worker located in this process.

// Remote call forwarding a reference to a local worker to a different process.
try await remoteWorker.introduce(another: charlie)
```

Three methods drive the lifecycle (`assignID`, `actorReady`, and `resignID`)
of every distributed actor that your system manages.

The Swift runtime calls these methods whenever a local distributed actor is initialized
or deinitialized. Implement these methods so the actor system can find actors by their
identity later:

```swift
extension SampleActorSystem {
    public func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor, Act.ID == ActorID {
        // Produce a unique ID for a freshly initializing actor.
        SampleActorID(node: self.node, instance: UUID())
    }

    public func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ID == ActorID {
        // Store a weak reference so the system does not keep the actor alive.
        activeActors.withLock { $0[actor.id] = WeakActorRef(actor) }
    }

    public func resignID(_ id: ActorID) {
        // Called when the actor is deinitialized or fails to initialize.
        activeActors.withLock { $0.removeValue(forKey: id) }
    }
}
```

Hold readied actors with weak references so they deinitialize when no other reference
remains. You can also hold some actors strongly if they remain valid for the entire
lifetime of the actor system, but weak retention is the common practice.

The Swift runtime automatically calls ``Distributed/DistributedActorSystem/resignID(_:)``
from a distributed actor's `deinit` to notify the system.

Inside `resignID`, typical work includes tearing down unused connections and freeing
per-actor resources such as caches or timers.

### Resolve local and remote actors

Resolving actor identifiers — or *resolving actors* for short — converts an `ActorID`
into a distributed actor of a specific type you can call distributed methods on. The
runtime resolves actors transparently when you pass distributed actor references as
parameters in distributed function calls, but you can also resolve them manually.

When you call `try Worker.resolve(id:using:)`, the runtime calls the
``Distributed/DistributedActorSystem/resolve(id:as:)`` method on the actor system
associated with the `Worker` type.

The user-facing `DistributedActor.resolve` function returns a local actor instance if
the identifier refers to an actor in the same process, or a remote proxy object if the
identifier points to a remote process. It can also throw an error if the identifier has
expired, is invalid, or is otherwise illegal.

The actor system's `resolve` implementation should return one of:

- The local instance, if the `ID` identifies an actor that this actor system created
  and manages.
- `nil`, if the actor isn't known to the system locally. In this case, the Swift runtime
  constructs a remote *proxy object pointing at an actor identified by this `ID`* instead.

```swift
extension SampleActorSystem {
    public func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
    where Act: DistributedActor, Act.ID == ActorID {
        guard let anyActor = activeActors.withLock({ $0[id]?.actor }) else {
            // Not local; if you have a connection to this node, form a remote reference.
            guard knownNodes.contains(id.node) else {
                throw SampleTransportError.unknownNode(id.node)
            }
            return nil
        }
        guard let actor = anyActor as? Act else {
            throw SampleTransportError.typeMismatch(expected: "\(Act.self)")
        }
        return actor
    }
}
```

The `resolve` function is synchronous; don't perform long blocking operations such as
communicating with a remote node to confirm the actor exists. Instead, return a reference
(or `nil`) quickly. If the remote target doesn't exist, the caller's first remote call
fails. This approach is preferable because the remote node could terminate between lookup
and first call, so a preflight check provides no real safety guarantee.

You can use `resolve` to initiate a remote connection, but don't block and wait for the
connection to fully establish before returning the reference.

### Encode an outgoing invocation

When you call a `distributed func` on a remote actor reference, the Swift runtime
creates a new `InvocationEncoder` by calling the system's
``Distributed/DistributedActorSystem/makeInvocationEncoder()`` method, then encodes the
arguments of the method call into it.

The runtime then passes the invocation encoder to `remoteCall`, which performs the remote
procedure call.

This ``Distributed/DistributedTargetInvocationEncoder`` must implement five recording
methods that the runtime invokes to record a method invocation into the actor system's
specific serialization format.

This sample implementation serializes each argument independently with `JSONEncoder`.
You can perform any kind of serialization here, including efficient binary encodings.

```swift
public struct SampleInvocationEncoder: DistributedTargetInvocationEncoder {
    public typealias SerializationRequirement = any Codable

    var genericSubstitutions: [String] = []
    var argumentData: [Data] = []
    var returnType: String?
    var errorType: String?

    public mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {
        // You may choose to throw here to ban generic distributed calls.
    }

    public mutating func recordArgument<Value: Codable>(
        _ argument: RemoteCallArgument<Value>
    ) throws {
        // Naive implementation — just encode every parameter independently.
        argumentData.append(try JSONEncoder().encode(argument.value))
    }

    public mutating func recordReturnType<R: Codable>(_ type: R.Type) throws {
        // Encoding the return type isn't required, but you can validate it here.
    }

    public mutating func recordErrorType<E: Error>(_ type: E.Type) throws {
        // Encoding the error type isn't required, but you can inspect the declared thrown type of the distributed func.
    }

    public mutating func doneRecording() throws {
        // Finalize the envelope — for example, compute a checksum or sign the payload.
    }
}
```

You can defer all serialization until `doneRecording`, or eagerly serialize each parameter
in the `record...` methods — whichever fits your serialization mechanism best.
If a later step needs the encoded arguments — for example, to compute a length prefix —
produce them in `doneRecording`.

### Call a remote method

After encoding the invocation, the runtime passes it to
``Distributed/DistributedActorSystem/remoteCall(on:target:invocation:throwing:returning:)``
(or the `Void` overload,
``Distributed/DistributedActorSystem/remoteCallVoid(on:target:invocation:throwing:)``).

In this method, the actor system sends the remote message and suspends the caller with
a continuation until a reply arrives.

The implementation of this method depends entirely on your transport. For example, you
can serialize the invocation into a WebSocket message, use an interprocess communication
mechanism on the same device, or send it over any other channel. The language feature
and runtime have no opinion on how you implement a remote call. You can use any networking
library, as long as `remoteCall` returns once the remote call completes.

A typical implementation associates each invocation with a continuation — keyed by an
invocation identifier or per-invocation stream — and resumes the continuation when a
reply arrives.

```swift
extension SampleActorSystem {
    public func remoteCall<Act, Err, Res>(
        on actor: Act,
        target: RemoteCallTarget,
        invocation: inout InvocationEncoder,
        throwing: Err.Type,
        returning: Res.Type
    ) async throws -> Res
    where Act: DistributedActor, Act.ID == ActorID,
          Err: Error, Res: Codable {
        let envelope = SampleEnvelope(
            recipient: actor.id,
            target: target.identifier,
            arguments: invocation.argumentData,
            substitutions: invocation.genericSubstitutions
        )

        // Here you can do additional work, such as enforcing timeouts or
        // propagating task-local values and distributed traces.

        return try await withCheckedThrowingContinuation { cc in
            // Your networking code here:
            someNetworkingLibrary.lowLevelSend(envelope) { reply in
                switch reply {
                case .success(let response):
                    cc.resume(returning: response.getAs(Res.self))

                case .error(let error):
                    // For example, network failures or timeouts.
                    cc.resume(throwing: error)
                }
            }
        }
    }

    // Invoked when the target 'distributed func' returns 'Void'.
    public func remoteCallVoid<Act, Err>(
        on actor: Act,
        target: RemoteCallTarget,
        invocation: inout InvocationEncoder,
        throwing: Err.Type
    ) async throws
    where Act: DistributedActor, Act.ID == ActorID, Err: Error {
        let envelope = SampleEnvelope(
            recipient: actor.id,
            target: target.identifier,
            arguments: invocation.argumentData,
            substitutions: invocation.genericSubstitutions
        )

        // Here you can do additional work, such as enforcing timeouts or
        // propagating task-local values and distributed traces.

        return try await withCheckedThrowingContinuation { cc in
            // Your networking code here:
            someNetworkingLibrary.lowLevelSend(envelope) { reply in
                switch reply {
                case .success:
                    cc.resume()

                case .error(let error):
                    // For example, network failures or timeouts.
                    cc.resume(throwing: error)
                }
            }
        }
    }
}
```

When your actor system surfaces transport failures such as timeouts or network errors,
conform those error types to ``Distributed/DistributedActorSystemError``. This marker
protocol lets developers distinguish transport-level failures from errors that a
distributed method throws itself, and the protocol documentation requires this
conformance for failures outside the caller's control.

> Note: Typed throws aren't supported in distributed function calls.

```swift
public enum SampleTransportError: DistributedActorSystemError {
    case processTerminated(node: String)
    case versionMismatch(remote: String)
}
```

### Decode and execute a remote invocation

Assemble a `receive` flow that dispatches incoming invocations to local actors.
On the receiving "remote" side of a call, decode the envelope into a matching
``Distributed/DistributedTargetInvocationDecoder``, locate the local actor, and hand both
to the runtime's ``Distributed/DistributedActorSystem/executeDistributedTarget(on:target:invocationDecoder:handler:)`` method.

Prepare a few simple types to make the call on the local distributed actor:

- Locate the **local actor** the call targets; this is where you use a `resolve()`-style
  method on your managed actors.
- Prepare an instance of your `DistributedTargetInvocationDecoder` type. Swift calls it
  when decoding the call's parameters.
- Prepare a callback wrapper `ResultHandler` that the runtime calls when the call
  completes.
  - The runtime calls this handler with the appropriate generic type.

```swift
extension SampleActorSystem {
    func receive(_ envelope: SampleEnvelope) async {
        do {
            // Resolve the local target actor for which the invocation was intended.
            let actor: any DistributedActor = try self.myResolveLocal(id: envelope.actorID)

            // Prepare a decoder over the network format that the Swift runtime calls.
            var decoder = SampleInvocationDecoder(envelope: envelope)

            // Prepare a handler that the runtime calls when the remote call completes.
            let handler = SampleResultHandler(replyTo: envelope.replyID)

            // Execute the 'distributed func' on the located target,
            // identified by the target identifier encoded during 'remoteCall'.
            try await executeDistributedTarget(
                on: actor,
                target: RemoteCallTarget(envelope.target),
                invocationDecoder: &decoder,
                handler: handler
            )
        } catch {
            // Send an error back if your transport supports it,
            // or terminate the connection. The choice depends on your transport.
            await envelope.errorChannel.fail(error)
        }
    }
}
```

The Swift distributed runtime calls the decoder with the appropriate generic type for
each argument. A `distributed` method with `String` and `Int` parameters causes the
runtime to invoke `decodeNextArgument` twice — once with `Argument` bound to `String`,
and once with `Argument` bound to `Int`. This lets the implementation rely on `Codable`
for decoding without unsafe casting or type guessing.
You can also use any other serialization scheme here, as long as it matches how the
values were originally encoded on the sending side.

```swift
public struct SampleInvocationDecoder: DistributedTargetInvocationDecoder {
    public typealias SerializationRequirement = any Codable

    let envelope: SampleEnvelope
    var nextArgumentIndex = 0

    public mutating func decodeGenericSubstitutions() throws -> [Any.Type] {
        [] // Implement only if you plan to support generic calls.
    }

    public mutating func decodeNextArgument<Argument: Codable>() throws -> Argument {
        defer { nextArgumentIndex += 1 }
        return try JSONDecoder().decode(Argument.self, from: envelope.arguments[nextArgumentIndex])
    }

    public mutating func decodeErrorType() throws -> Any.Type? { nil }
    public mutating func decodeReturnType() throws -> Any.Type? { nil }
}
```

You can ignore the generic substitutions and return/error type decoding unless you support
generic distributed function calls. The runtime supports them, but you need to rely on
mangled type names or another mechanism to transport the intended generic arguments to
the recipient.

> Tip: If you support generic distributed calls, don't trust incoming types. Validate
> them against a known allow-list before decoding.

### Reply with the results and errors

The final step of a remote call chain is the ``Distributed/DistributedTargetInvocationResultHandler``
that you use to return results and errors.

This works similarly to the encoder and decoder, and gives you a type-safe way to obtain
the result with its correct `Success` type, as declared by the distributed method.

If the method returns `Void`, the runtime calls the specialized `onReturnVoid()` instead.
If the target method throws an error, the handler receives that error in the
`onThrow(error:)` callback, where you decide how to respond — for example, shut down the
connection, or send back a "remote call failed" error.

```swift
public struct SampleResultHandler: DistributedTargetInvocationResultHandler {
    public typealias SerializationRequirement = any Codable

    let replyTo: SampleReplyChannel

    public func onReturn<Success: Codable>(value: Success) async throws {
        try await replyTo.succeed(JSONEncoder().encode(value))
    }

    public func onReturnVoid() async throws {
        try await replyTo.succeedVoid()
    }

    public func onThrow<Err: Error>(error: Err) async throws {
        // Not every Error is Codable; encode it when possible, and fall back otherwise.
        if let codable = error as? (any Codable & Error) {
            try await replyTo.fail(encoded: JSONEncoder().encode(codable))
        } else {
            try await replyTo.fail(message: "\(error)")
        }
    }
}
```

### Use the actor system

The actor system abstracts the serialization and networking details so the use-site code can
focus on its business domain. Specify the `ActorSystem` typealias inside a distributed actor.

```swift
distributed actor Greeter {
    typealias ActorSystem = SampleActorSystem

    distributed func hello(name: String) -> String {
        "Hello, \(name)!"
    }
}
```

Initialize the system and distributed actor, use `resolve` to get a reference, and
make asynchronous calls that you defined on the distributed actor:

```swift
let system = SampleActorSystem(localNode: "node-a")
let local = Greeter(actorSystem: system)

// In a remote deployment, resolving an ID owned by a remote node returns a remote
// reference. Here, both sides share one in-process system, so this example returns
// a local instance.
let remote = try Greeter.resolve(id: local.id, using: system)
let reply = try await remote.hello(name: "world")
print(reply) // "Hello, world!"
```

For more information, see ``Distributed/DistributedActorSystem`` and
``Distributed/LocalTestingDistributedActorSystem``.