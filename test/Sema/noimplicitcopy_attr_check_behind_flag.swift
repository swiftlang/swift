// RUN: %target-typecheck-verify-swift -parse-stdlib -disable-availability-checking

class Klass {}

func useKlass(_ k: Klass) {}

// Make sure that we properly error when someone uses no implicit copy without
// setting -enable-experimental-move-only.
func letDecls(_ x: Klass) -> () {
    @_noImplicitCopy let y: Klass = x // expected-error {{Can not use feature when experimental move only is disabled! Pass the frontend flag -enable-experimental-move-only to swift to enable the usage of this language feature}}
    useKlass(y)
}
