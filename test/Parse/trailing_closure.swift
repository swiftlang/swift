// RUN: %target-typecheck-verify-swift

func f()->Int{ 42 }

//This should be interpreted as a trailing closure, instead of being interpreted as a computed property with undesired initial value.
struct S {
    var x : Int = f () { // expected-error {{argument passed to call that takes no arguments}}
        3
    }
}
