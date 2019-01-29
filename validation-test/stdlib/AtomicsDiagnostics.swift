// RUN: %target-typecheck-verify-swift

import SwiftPrivate

class AtomicIntSubclass : _stdlib_AtomicInt {} // expected-error {{inheritance from a final class '_stdlib_AtomicInt'}}

