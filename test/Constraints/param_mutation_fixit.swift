// RUN: %target-typecheck-verify-swift

func simpleAssignment(x: Int) {
    x = 3
    // expected-error@-1 {{cannot assign to value: 'x' is a 'let' constant}}
    // expected-note@-2 {{add 'var x = x' to make a mutable copy}} {{1-1=var x = x\n}}
}

func compoundAssignment(x: Int) {
    x += 3
    // expected-error@-1 {{left side of mutating operator isn't mutable: 'x' is a 'let' constant}}
    // expected-note@-2 {{add 'var x = x' to make a mutable copy}} {{1-1=var x = x\n}}
}

// Regular let variables should still get the 'let -> var' fix-it, not this one.
func localLet() {
    let x = 3  // expected-note {{change 'let' to 'var' to make it mutable}}
    x = 4  // expected-error {{cannot assign to value: 'x' is a 'let' constant}}
}
