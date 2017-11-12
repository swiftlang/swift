// RUN: %target-typecheck-verify-swift

class AtomicIntSubclass : _stdlib_AtomicInt {} // expected-error {{inheritance from a final class '_stdlib_AtomicInt'}}

