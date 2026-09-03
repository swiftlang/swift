# `@remoteCall`

Express semantic attributes that the actor system should take into account when executing remote calls.

## Overview

By default, distributed actor method calls follow the same request/response
semantic locally as well as for remote calls - consistently modelling how 
normal method calls work in Swift, by awaiting them until the result is produced.

The `@remoteCall` attribute allows modifying the semantics of method calls
when they happen to be remote calls. In other words, if the call is made
on a local actor instance, normal actor semantics are in play since e.g.
the latency and cost of making those calls is the same as with normal actor calls.
However, remote calls have different cost tradeoffs and latency implications,
so it is sometimes necessary to express specific semantic expectations for those. 

This attribute is provided to the actor system hosting the actor on which a call is made,
and it **may** act on these semantic hints. Some transports may outright reject some
semantics, e.g. many systems will not support blocking calls and fail calls made which
semantics that the system does not support, or simply ignore the semantic hint.

> Note: The `@remoteCall` attribute has no effect on local actor calls.
> Calls to local actor instances still respect the async/await until method completion semantics,
> and only remote calls may be able to optimize their execution in other ways due to the process boundary separating 
> client and server side of such a call.

## Oneway remote calls

The `oneway` remote semantic attribute enables API authors to express the expectation that 
a remote call does not need to await the response from the remote on which the call is made.

This matches the "message send" semantics found in other messaging systems, 
rather than the usual "request/response" semantics of default distributed actor methods.
Oneway method calls may be implemented by an actor system using more efficient implementation techniques,
however they cannot carry _any_ form of response from the remote side back to the caller. 

> Note: Oneway method calls are "fire-and-forget", they cannot carry back any response from the remote side of the call.

To declare an oneway method, you annotate a distributed method like this:

```swift
distributed actor Greeter { 
  @remoteCall(oneway)
  distributed func thanks()
}
```

Oneway methods must be  `Void`-returning. Distributed computed properties cannot be oneway, because inherently
computed properties are intended to express the notion of reading the property's value.

Oneway method do have the drawback that they do not exhort _any_ natural flow control or back-pressure,
and either the actor system, or the end-user may need to employ some flow control patterns in order to 
avoid overwhelming the server-side of such calls. The details of this are highly system and transport dependent,
so please read the documentation of the specific actor system for further details.

The actor system receives this semantics hint via the ``Distributed/RemoteCallTarget/isOnewayRemoteCall`` property,
on the ``Distributed/RemoteCallTarget`` passed to the ``Distributed/DistributedActorSystem/remoteCallVoid`` method
when a remote call is performed.

## Synchronous blocking remote calls

> Warning: Blocking calls should be used with extreme caution. Most actor systems
> are expected to not implement support for blocking requests, 
> and either should either silently ignore this attribute, 
> or fail such requests explicitly stating lack of support for these semantics.
> 
> Actor systems which do implement these semantics usually have some transport specific
> specialized reasons and important rationale for doing so.

The `blocking` semantics attribute requests the actor system runtime to perform a request/response
using a synchronous, blocking, messaging primitive. Such primitives are usually not recommended, 
or outright impossible in networking libraries however may be important in very specific forms of
IPC (Inter-Process Communication), where dedicated synchronous primitives may have privileged execution semantics.

To declare a blocking remote-call method, you annotate a distributed method like this:

```swift
@remoteCall(blocking)
distributed func superQuickRender() -> Frame
```

The distributed method call is still `async throws`, as the message sending side of the call
still may need to suspend or throw when the IPC is performed. 

If the call is local, and no process boundary is being crossed, the method's execution is the
same as an ordinary actor call, and may suspend in order to obtain the target actor's isolation.

The target actor system may validate the calling context, and for example only allow calling a blocking
remote call method from a specific task executor that is dedicated to blocking operations, in order to
guard the global concurrency pool from potential thread starvation issues. Please refer to concrete
actor system documentation to learn about the system's requirements for handling remote blocking calls.

The actor system receives this semantics hint via the ``Distributed/RemoteCallTarget/isSynchronousBlockingRemoteCall`` property,
on the ``Distributed/RemoteCallTarget`` passed to the ``Distributed/DistributedActorSystem/remoteCall`` method
when a remote call is performed.

## Combining semantics

In principle, multiple `@remoteCall` attributes may be stacked on a single declaration.

However, currently the two offered semantics (`oneway` and `blocking`) are mutually exclusive.
A call that discards its reply cannot also be a synchronously waiting for a response.

In the future more semantics may be introduced, which would be compatible with either of the existing ones.

## Remote call semantics and resolvable protocols 

Since distributed actor APIs are most often expressed as a shared module/package where the public API surface
of a service is declared using a `protocol`, remote call semantics may be expressed directly on protocol requirements:

```swift
@Resolvable 
protocol Greeter where ActorSystem == ExampleActorSystem {
  @remoteCall(oneway)
  distributed func thanks()
} 
```

These semantics apply both to the generated proxy type (`$Greeter`) on which such remote calls can be made from a client
that does not know the server's implementation type (e.g. `GreeterImpl`). 

The declared remote call semantics are also effectively inherited by any conforming distributed actor type,
such that it is not necessary to re-state the same semantics in the conforming type:

``````swift
distributed actor GreeterImpl: Greeter {
  distributed func thanks() { } // caller semantics remain @remoteCall(oneway)
} 
```

If someone were to make a `try await thanks()` remote call on a remote reference to the `GreeterImpl` type,
this call would also have the expected oneway semantics.

The semantics do not apply for any local calls, so calling methods on an actual local actor instance of
`GreeterImpl` still respect the usual async/await until method completion semantics.

## Topics

### Handling remote call semantics

- ``Distributed/RemoteCallTarget/isOnewayRemoteCall``
- ``Distributed/RemoteCallTarget/isSynchronousBlockingRemoteCall``
