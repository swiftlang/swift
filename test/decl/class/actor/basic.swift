// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

// REQUIRES: concurrency

actor MyActor { }

class MyActorSubclass1: MyActor { }
// expected-error@-1{{actor types do not support inheritance}}
// expected-error@-2{{type 'MyActorSubclass1' cannot conform to the 'Actor' protocol}}
// expected-error@-3{{non-final class 'MyActorSubclass1' cannot conform to `Sendable`; use `UnsafeSendable`}}

actor MyActorSubclass2: MyActor { } // expected-error{{actor types do not support inheritance}}

// expected-error@+1{{keyword 'class' cannot be used as an identifier here}}
actor class MyActorClass { }

class NonActor { } // expected-note{{overridden declaration is here}}

actor NonActorSubclass : NonActor { } // expected-error{{actor types do not support inheritance}}
// expected-error@-1{{actor-isolated initializer 'init()' has different actor isolation from nonisolated overridden declaration}}

// expected-error@+1{{keyword 'class' cannot be used as an identifier here}}
public actor class BobHope {}
// expected-error@+1{{keyword 'public' cannot be used as an identifier here}}
actor public class BarbraStreisand {}
// expected-error@+1{{keyword 'struct' cannot be used as an identifier here}}
public actor struct JulieAndrews {}
// expected-error@+1{{keyword 'public' cannot be used as an identifier here}}
actor public enum TomHanks {}
