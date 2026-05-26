# Distributed module implementation notes

This document is aimed at developers working in the `Distributed` module,
and serves as a design document of the runtime internals. 

> These are implementation details and are allowed to change without further notice.
> Please refer to Swift evolution proposals for the public and documented language 
> aspects of the Distributed module.

## Distributed calls with `any/some P` where `P` is `@Resolvable protocol`

Resolvable protocols are special in the sense that they allow a remote peer to refer to
an actor on another host without knowing its _actual_ type.

For example, a **server** may be hosting:

```swift
distributed actor PolishImpl: Greeter {
   distributed func greet() -> String { "Cześć!" }
}
```

and the only shared information between server and client is the protocol:

```swift
@Resolvable
protocol Greeter: DistributedActor where ActorSystem == SomeSystem {
   distributed func greet() -> String 
}
```

This allows the client side to resolve a "proxy" (or sometimes called "stub") remote reference, 
by using the synthesized `$Greeter` type:

```swift
let remoteRef: any Greeter = try $Greeter.resolve(<id>, using: system)
```

Next, we want to be able to share these references across remote calls, like this:

```swift
distributed actor CallCenter {
  distributed func callMeLater(_ who: any Greeter)
}
```

This allows us to implement distributed "callbacks", because we can send a remote peer a reference to an actor that they 
should invoke at a later point in time. This is a fundamental building block for all kinds of bi-directional communication.

> Note: Of course, there must be some validation if we allow given type to be serialized and cross network boundaries,
> however these checks are up to the system implementation (in `resolve` and in the transport layer), and not up to the 
> language layer which only enforces static concepts.

Here, we want to allow callers to pass _any_ distributed actor that conforms to the `Greeter` protocol.

Without special treatment, this is not supported, because the `any Greeter` protocol itself does cannot conform
to e.g. the `Codable` serialization requirement of a system.

We could also try to send a generic actor, which is technically supported, as long as the system transports the generic type,
and vets it against an allow list etc:

```swift
distributed actor CallCenter {
  distributed func callMeLater(_ who: some Greeter)
}
```

Technically this is possible, and the system would just encode the `PolishImpl` type and send it to the remote side.

This hits a problem though: The remote side does not know--nor do we want it to know--about the `PolishImpl` type!
Therefore trying to receive `PolishImpl` type, on a system which does not have it, would fail because we cannot create actor.

Distributed actors are never _actually_ serialized to begin with. We always serialize their ID, and as 
long as we can transfer that, we can transfer a "remote reference".

We also know that both server and client share the same `protocol Greeter`, and that they use the same actor system.
Therefore the availability of the `ActorID` type is guaranteed, as is the availability of the `$Greeter` proxy.

**The solution** is to encode any attempts to "send" an `any/some P` where the `P` as-if we were encoding the `$P`.
The recipient side shall then also decode it as-if we were receiving an `$P` and this way we never attempt to decode
unknown distributed actor types on the recipient.