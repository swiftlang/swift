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

- Swift language support for actors and distributed actors.
  For more information,
  see [Concurrency][concurrency] in [The Swift Programming Language][tspl].

- The Distributed module, which includes the types and protocols you need
  to declare and use distribute actors.
  For example, it has
  protocols to which distributed actors and distributed actor systems conform,
  and structures that encapsulate information about calls to a distributed actor.

- A *distributed actor system*, also called a cluster runtime,
  provides an implementation of the ``Distributed/DistributedActorSystem`` protocol
  and coordinates between the cluster's nodes.
  A distributed actor is always part of some distributed actor system;
  that distributed actor system handles the serialization and networking
  necessary to perform remote method calls.
  For local testing, you can use ``Distributed/LocalTestingDistributedActorSystem``.
  For production,
  you can use the distributed actor system
  from the [Swift Distributed Actors][runtime] library,
  use another library,
  or write your own distributed actor system.

[concurrency]: https://docs.swift.org/swift-book/LanguageGuide/Concurrency.html
[tspl]: https://docs.swift.org/swift-book/
[runtime]: https://github.com/apple/swift-distributed-actors/

## Topics


### Distributed Actors

- ``Distributed/DistributedActor``
- ``Distributed/DistributedActorSystem``
- ``Distributed/Resolvable()``
- ``Distributed/buildDefaultDistributedRemoteActorExecutor(_:)``

### Remote Calls

- ``Distributed/RemoteCallTarget``
- ``Distributed/RemoteCallArgument``
- ``Distributed/DistributedTargetInvocationEncoder``
- ``Distributed/DistributedTargetInvocationDecoder``
- ``Distributed/DistributedTargetInvocationResultHandler``

### Local Testing

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
