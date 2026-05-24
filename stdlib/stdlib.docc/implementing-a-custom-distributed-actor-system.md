# Implementing a Custom Distributed Actor System

Implement a `DistributedActorSystem` to provide a custom transport layer 
for distributed actors.

## Overview

A ``Distributed/DistributedActor`` is always associated with a *distributed actor system* 
which determines how `distributed func` calls on a _remote_ actor are executed.

To declare a distributed actor you can use the `distributed actor` pair of keywords,
and you will have to determine what actor system it is associated with. This effectively
means choosing a "transport", such as a network or inter-process transport, for your actor.

Various actor system implementations exist in the Swift ecosystem, but you are also able
to implement your own, in case you'd like to make remote procedure calls over some transport
mechanism that doesn't have an implementation available yet.

You do not need in-depth knowledge about an actor system's implementation to
just use distributed actors -- that is their point, to abstract and hide away the transport details --
you can implement the ``Distributed/DistributedActorSystem`` protocol yourself and provide
a new way for distributed actors to communicate.

Most code that *uses* distributed actors never interacts with this protocol
directly. You only need to implement a `DistributedActorSystem` when you are
building a transport - for example, a cluster, a WebSocket client/server, or
some other inter-process communication (IPC) system. 

> TIP: In other words, ``Distributed/DistributedActorSystem``
> is a way to write your own RPC frameworks, that are deeply 
> integrated in the Swift runtime and concurrency model.

An actor system is responsible for lifecycle management and remote interactions of distributed actors.
These responsibilities roughly fall into one of the following categories, which have their corresponding 
methods on the `DistributedActorSystem` protocol:

- **Assign**, track, and release actor identities.
- **Resolve** an actor identity to either a local instance or a remote reference.
- Perform a **remote call** on a remote distributed actor. 
- **Encode** an outgoing invocation, send it to the remote peer, and await a reply.
- **Decode** an incoming invocation, dispatch it to the target actor, and send
  the result back.

The rest of this article walks through each responsibility and ends with a
minimal in-memory transport. For the deeper language
and runtime semantics, see [SE-0336: Distributed Actor Isolation][SE-0336] and
[SE-0344: Distributed Actor Runtime][SE-0344].

[SE-0336]: https://github.com/apple/swift-evolution/blob/main/proposals/0336-distributed-actor-isolation.md
[SE-0344]: https://github.com/apple/swift-evolution/blob/main/proposals/0344-distributed-actor-runtime.md

### The Distributed Actor System and Associated Types

A `DistributedActorSystem` is usually a final class (or struct, wrapping a class), 
because it is an inherently stateful object referenced by identity and retains internal
state such as identifier-to-actor mappings. 

To implement a distributed actor system declare a new type and conform it to the ``Distributed/DistributedActorSystem`` protocol.
Then, provide witnesses for the five required associated types, which we will discuss one by one next. 

```swift
import Distributed

public final class SampleActorSystem: DistributedActorSystem {
    public typealias ActorID = SampleActorID
    public typealias SerializationRequirement = any Codable
    public typealias InvocationEncoder = SampleInvocationEncoder
    public typealias InvocationDecoder = SampleInvocationDecoder
    public typealias ResultHandler = SampleResultHandler

    // Internal state filled in below
}
```

### Manage Actor Identity

The `ActorID` serves as an identifier that a distributed actor is assigned at creation time,
and is going to be serialized and sent to other remote peers when making network calls involving distributed actors.
This identifier is what enables sending "references" to an actor to other nodes or processes, as the recipient will then
``Distributed/DistributedActor/resolve(id:using:)`` the identifier to obtain as _remote reference_ to the actor.

```swift
public struct SampleActorID: Hashable, Sendable, Codable {
    public let node: String
    public let instance: UUID
}
```

By making the `SampleActorID` conform to `Codable`, we have made it compatible with the actor system's
`SerializationRequirement`.

Because ``Distributed/DistributedActor`` is ``Codable`` whenever its `ID` is,
choosing a `Codable` `ActorID` is what makes distributed actors serializable
as arguments to other distributed calls, in other words, we will be able to make remote calls like this:

```swift
distributed actor Worker { 
  distributed func introduce(another: Worker) { ... }
}

let remoteWorker: Worker = ... // worker located in different process
let charlie: Worker = Worker(actorSystem: ...) // worker located in this process

// Remote call forwarding a reference to a local worker, to a different process
try await remoteWorker.introduce(another: charlie)
```

Three methods drive the lifecycle of every distributed actor that your system
manages. 

The Swift runtime will call these methods whenever a local distributed actor is initialized (or deinitialized).
We need to implement these methods in a way that will enable the actor system to find those actors by their identity
in the future:

```swift
extension SampleActorSystem {
    public func assignID<Act>(_ actorType: Act.Type) -> ActorID
      where Act: DistributedActor, Act.ID == ActorID {
        // Produce a unique id for a freshly-initializing actor
        SampleActorID(node: self.node, instance: UUID())
    }

    public func actorReady<Act>(_ actor: Act)
      where Act: DistributedActor, Act.ID == ActorID {
        // Store a weak reference so the system does not keep the actor alive
        activeActors.withLock { $0[actor.id] = WeakActorRef(actor) }
    }

    public func resignID(_ id: ActorID) {
        // Called when the actor is deinitialized or failed to initialize
        activeActors.withLock { $0.removeValue(forKey: id) }
    }
}
```

Retain readied actors *weakly* so that they can be deinitialized when no
remaining reference holds them. You may also choose to retain some actors 
strongly, if they are going to be valid for the entire lifetime of this actor system,
but weakly retaining actors is by far the more common practice. 

The system is notified when a distributed actor is deinitialized through the
``Distributed/DistributedActorSystem/resignID(_:)`` call, invoked from a distributed 
actor's deinit automatically by the Swift runtime.

Typical tasks to perform inside `resignID` are tearing down no-longer used connections and 
freeing up resources associated with an actor, such as caches or timers.

### Resolve Local and Remote Actors

Resolving actor identifiers, or resolving actors for short, is the primary way 
to convert an `ActorID` into a distributed actor of some specific type, 
that you can call distributed methods on. Often, this is done transparently
as you pass distributed actor references as parameters in distributed function calls,
but it is possible to perform this manually as well.

When you call `try Worker.resolve(id:using:)`, the runtime calls into
``Distributed/DistributedActorSystem/resolve(id:as:)`` of the actor system associated with the `Worker` type.

The user-facing `DistributedActor.resolve` function is defined to return either a local actor instance,
if the identifier is of an actor in the same process, or it will return a remote proxy object
if the identifier points at a remote process. It may also throw, if the identifier is in any way expired,
invalid, or illegal in any way.

The actor system's resolve implementation however should return either: 
- the local instance if the id identifies an actor that was created with, and is managed by, 
  this actor system.
- or `nil`, if the actor is not known to the system locally, 
  and the Swift runtime should construct a remote _proxy object pointing at an actor identified by this id_ instead.

```swift
extension SampleActorSystem {
    public func resolve<Act>(id: ActorID, as actorType: Act.Type) throws -> Act?
    where Act: DistributedActor, Act.ID == ActorID {
        guard let anyActor = activeActors.withLock({ $0[id]?.actor }) else {
            // Not local, but we have a connection to this node, form remote reference
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

The `resolve` function is synchronous and should not perform long blocking operations, 
such as actually communicating with a remote node or process it that actor truly exists remotely. 
Instead, it should quickly return a reference (or nil), and if the remote target happens to not exist
the caller will be notified about this during their first remote call, which will fail. 
This approach is better, because the remote node may terminate between the time of lookup an first 
call, so we did not really gain any safety by doing these pre-flight checks.

You may use the resolve to initiate a remote connection, however it is not recommended to block and
wait for that connection to establish fully before returning the reference.

### Encode a Remote Invocation

Whenever a `distributed func` is called on a remote actor reference, the Swift runtime
will create a new `InvocationEncoder` by calling its system's 
``DistributedActorSystem/makeInvocationEncoder()`` method, 
and encode all the arguments of the method call into it.

Then this invocation encoder is passed to the `remoteCall` which will perform the actual remote procedure call.

This ``Distributed/DistributedTargetInvocationEncoder`` must implement five
recording methods, which the runtime will then invoke in order to record a method 
invocation into the actor system's specific serialization format.

This simple sample implementation uses mangled names and just JSON serialization per element,
however you can perform all kinds of serialization here, including efficient binary encodings.

```swift
public struct SampleInvocationEncoder: DistributedTargetInvocationEncoder {
    public typealias SerializationRequirement = any Codable

    var genericSubstitutions: [String] = []
    var argumentData: [Data] = []
    var returnType: String?
    var errorType: String?

    public mutating func recordGenericSubstitution<T>(_ type: T.Type) throws {
        // you may choose to throw here to ban generic distributed calls
    }

    public mutating func recordArgument<Value: Codable>(
        _ argument: RemoteCallArgument<Value>
    ) throws {
        // Naive implementation, just encode every parameter independently
        argumentData.append(try JSONEncoder().encode(argument.value))
    }

    public mutating func recordReturnType<R: Codable>(_ type: R.Type) throws {
        // not required to encode, however you can validate the return type here
    }

    public mutating func recordErrorType<E: Error>(_ type: E.Type) throws {
        // not required to encode, however you can inspect the declared thrown type of the distributed func
    }

    public mutating func doneRecording() throws {
        // Finalize the envelope, e.g. compute a checksum or sign the payload
    }
}
```

You can delay any actual serialization work until the `doneRecording` call, 
or you may eagerly serialize each parameter inside the `record...` calls, 
whichever fits your serialization mechanism of choice better.
If a subsequent step needs the encoded arguments (for example, a length
prefix), compute it here.

### Perform a Remote Call

Once all this is done, the invocation is encoded and will be passed to the 
``Distributed/DistributedActorSystem/remoteCall(on:target:invocation:throwing:returning:)``
(or the `Void` overload,
``Distributed/DistributedActorSystem/remoteCallVoid(on:target:invocation:throwing:)``).

Here, the actor system should perform the remote message send, 
and suspend the caller with a continuation until a reply arrives.

The implementation of this method is inherently tied to the exact transport layer details
of the underlying transport mechanism. For example, you may serialize the invocation
into a websocket message, use some cross-process communication mechanism on the same device,
or use something else entirely to make the remote invocation. The language feature and runtime
have no opinion on how a remote call is to be implemented, and you are free to use your
favorite networking libraries here, as long as when the call completes, the `remoteCall` returns.

Typically, an implementation would use invocation identifiers, or per invocation streams,
and then associate a continuation with it. Resuming it when a reply arrives.

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
      
        // Here you can do any additional tasks, such as timeouts, 
        // task-local or distributed-trace propagation.
      
        return try await withCheckedContinuation { cc in
          // Your networking code here:
          someNetworkingLibrary.lowLevelSend(envelope) { reply in
            switch reply {
              case .success(let response):
                cc.resume(returning: response.getAs(Res.self))
        
              case .error(let error):
                // e.g. network failures or timeouts
                cc.resume(throwing: error)
            }
          }
        }
    }

    // Invoked when the called 'distributed func' returns 'Void'
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

      // Here you can do any additional tasks, such as timeouts, 
      // task-local or distributed-trace propagation.

      return try await withCheckedContinuation { cc in
        // Your networking code here:
        someNetworkingLibrary.lowLevelSend(envelope) { reply in
          switch reply {
          case .success:
            cc.resume() 

          case .error(let error):
            // e.g. network failures or timeouts
            cc.resume(throwing: error)
          }
        }
      }
    }
}
```

If your actor system injects failures, such as timeouts or network failures, 
it is recommended to make them conform to the 
``Distributed/DistributedActorSystemError`` protocol. This marker protocol lets users
distinguish transport-level failures from errors a distributed method threw
itself, and it is the conformance the protocol documentation asks you to adopt
for "outside the user's control" failures.

> Tip: Typed throws are currently not supported in distributed function calls.

```swift
public enum SampleTransportError: DistributedActorSystemError {
  case processTerminated(node: String)
  case versionMismatch(remote: String)
}
```

### Receive and Execute an RemoteInvocation

Next, on the receiving "remote" side of a call, decode the envelope into a matching
``Distributed/DistributedTargetInvocationDecoder`` and hand it to the runtime's
`executeDistributedTarget`. 

Finally, you need to prepare a few simple types to make the call on the local distributed actor:

- Locate the **local actor** the distributed call was intended to; this is where you'd use a `resolve()` style method on your managed actors.
- Prepare an instance of your `DistributedTargetInvocationDecoder` type, it will be called by Swift when decoding call parameters.
- Prepare a simple callback wrapper `ResultHandler` which will be invoked when the distributed function call completes,
  - this result handler will be called with the correct generic type

```swift
extension SampleActorSystem {
    func receive(_ envelope: SampleEnvelope) async {
        do {
            // Resolve the local target actor for which the invocation was intended
            let actor: any DistributedAtor = try self.myResolveLocal(id: envelope.actorID)
          
            // Prepare a decoder from the network format into a Decoder the Swift runtime will invoke
            var decoder = SampleInvocationDecoder(envelope: envelope)
          
            // Prepare a handler which will be called when the remote call completes
            let handler = SampleResultHandler(replyTo: envelope.replyID)

            // Execute the 'distributed func' on the located target,
            // identified by the target identifier, that we encoded during 'remoteCall' 
            try await executeDistributedTarget(
                on: actor,
                target: RemoteCallTarget(envelope.target),
                invocationDecoder: &decoder,
                handler: handler
            )
        } catch {
            // If able to, you may be able to send an error back, 
            // or just terminate the connection due to the error - depends on your specific transport.
            await envelope.errorChannel.fail(error)
        }
    }
}
```

The decoder is going to be called by the Swift distributed runtime with the appropriate generic type arguments
for every argument. A `distributed` method with a `String` and `Int` parameter will cause the runtime
to invoke the `decodeNextArgument` twice, once with the `Argument` type bound to `String`, and once to `Int`.
Thanks to this, the implementation can just rely on `Codable` for decoding easily, without any unsafe casting or guessing types.
You can also use any other serialization scheme here, as long as it matches how the values were originally encoded on the sending side.

```swift
public struct SampleInvocationDecoder: DistributedTargetInvocationDecoder {
    public typealias SerializationRequirement = any Codable

    let envelope: SampleEnvelope
    var nextArgumentIndex = 0

    public mutating func decodeGenericSubstitutions() throws -> [Any.Type] {
        [] // only implement if you plan on supporting generic calls
    }

    public mutating func decodeNextArgument<Argument: Codable>() throws -> Argument {
        defer { nextArgumentIndex += 1 }
        return try JSONDecoder().decode(Argument.self, from: envelope.arguments[nextArgumentIndex])
    }

    public mutating func decodeErrorType() throws -> Any.Type? { nil }
    public mutating func decodeReturnType() throws -> Any.Type? { nil }
}
```

You can ignore the generic substitutions and return/error type decoding, 
unless you intend to support generic distributed function calls. These are supported by the runtime,
but you would need to rely either on mangling type names, or on another mechanism to transport the
intended generic arguments to the recipient. 

> Tip: If you do decide to support generic distributed calls,
> please be aware that the types may not entirely be trusted, 
> and you should validate the to-be-decoded types against a known allow-list.

### Report Results and Errors

The final step of a remote call chain is the result handler.

This functions similar to the encoder and decoder we discussed before, but is intended to give you a type-safe
way to obtain the correct result `Success` type that a distributed method has returned.

If the method returns void, the specialized `onReturnVoid()` is called instead. 
If the target method throws an error, the handler will receive that error in the `onThrow(error:)` callback,
and you may choose how to act on the error accordingly to your preferences (e.g. shutdown the connection, or
send back some form of "remote call failed" error).

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
        // Not every Error is Codable; surface what we can and fall back otherwise
        if let codable = error as? (any Codable & Error) {
            try await replyTo.fail(encoded: JSONEncoder().encode(codable))
        } else {
            try await replyTo.fail(message: "\(error)")
        }
    }
}
```

### Putting It All Together

Once everything is wired together, you should be able to make your first remot ecall using
your newly built actor system.

We have greatly simplified and abstracted away any serialization and nerworking details
from the use-site code, which now can focus on your business comains and concepts:

```swift
distributed actor Greeter {
    typealias ActorSystem = SampleActorSystem

    distributed func hello(name: String) -> String {
        "Hello, \(name)!"
    }
}
```

```swift
let system = SampleActorSystem(localNode: "node-a")
let local = Greeter(actorSystem: system)

// Resolving the same id through the system returns a remote reference
let remote = try Greeter.resolve(id: local.id, using: system)
let reply = try await remote.hello(name: "world")
print(reply) // "Hello, world!"
```

It's worth experimenting and trying different ideas how to model your distributed, or client/server applications
using distributed actors! You might find that by sharing a lot of the underlying infrastructure, you'll free
up your application from un-necessary low level details.

In some application, where fine grained control over every byte and detail of every single request is necessary,
you may still choose to reach for raw low level networking primitives, use existing RPC systems, 
or design your own network protocols, however we encourage to embrace the Swift-first nature of actors when it suits your application.

Distributed actor systems have the benefit that once implemented, reusing them becomes simple. And you can even implement entire
distributed algorithms against the abstract notion of a distributed actor system, without specifying which exact transport
it is required to utilize.