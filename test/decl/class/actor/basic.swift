// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

// REQUIRES: concurrency

actor MyActor { }

class MyActorSubclass1: MyActor { } // expected-error{{actor types do not support inheritance}}
// expected-error@-1{{non-final class 'MyActorSubclass1' cannot conform to `Sendable`; use `UnsafeSendable`}}

actor MyActorSubclass2: MyActor { } // expected-error{{actor types do not support inheritance}}

// expected-error@+1{{keyword 'class' cannot be used as an identifier here}}
actor class MyActorClass { }

class NonActor { }

actor NonActorSubclass : NonActor { } // expected-error{{actor types do not support inheritance}}

// expected-error@+1{{keyword 'class' cannot be used as an identifier here}}
public actor class BobHope {}
// expected-error@+1{{keyword 'public' cannot be used as an identifier here}}
actor public class BarbraStreisand {}
// expected-error@+1{{keyword 'struct' cannot be used as an identifier here}}
public actor struct JulieAndrews {}
// expected-error@+1{{keyword 'public' cannot be used as an identifier here}}
actor public enum TomHanks {}
