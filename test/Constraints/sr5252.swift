// RUN: %target-typecheck-verify-swift

protocol P {}
class Helper {}

class Base {}
class Sub<T>: Base {}

// The superclass constraint was the culprit.
func foo<T: Helper & P>(base: Base, arg: T) {
    _ = base as? Sub<T>
}
