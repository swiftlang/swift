// RUN: %swift %s -parse -verify

class AtomicIntSubclass : _stdlib_AtomicInt {} // expected-error {{inheritance from a final class '_stdlib_AtomicInt'}}

