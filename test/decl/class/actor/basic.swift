// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

actor class MyActor { }

class MyActorSubclass1: MyActor { }

actor class MyActorSubclass2: MyActor { }

class NonActor { }

actor class NonActorSubclass : NonActor { } // expected-error{{actor class cannot inherit from non-actor class 'NonActor'}}
