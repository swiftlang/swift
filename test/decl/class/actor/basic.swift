// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

// REQUIRES: concurrency

actor MyActor { }

class MyActorSubclass1: MyActor { } // expected-error{{actor types do not support inheritance}}
// expected-error@-1{{non-final class 'MyActorSubclass1' cannot conform to `Sendable`; use `UnsafeSendable`}}

actor MyActorSubclass2: MyActor { } // expected-error{{actor types do not support inheritance}}

// expected-warning@+1{{'actor class' has been renamed to 'actor'}}{{7-13=}}
actor class MyActorClass { }

class NonActor { }

actor NonActorSubclass : NonActor { } // expected-error{{actor types do not support inheritance}}

// expected-warning@+1{{'actor class' has been renamed to 'actor'}}{{14-20=}}
public actor class BobHope {}
// expected-warning@+1{{'actor class' has been renamed to 'actor'}}{{14-19=actor}}{{1-7=}}
actor public class BarbraStreisand {}
// expected-warning@+2{{'actor class' has been renamed to 'actor'}}{{14-21=}}
// expected-error@+1{{'actor' may only be used on 'class' declarations}}
public actor struct JulieAndrews {}
// expected-warning@+2{{'actor class' has been renamed to 'actor'}}{{14-18=actor}}{{1-7=}}
// expected-error@+1{{'actor' may only be used on 'class' declarations}}
actor public enum TomHanks {}
