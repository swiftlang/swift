// RUN: %target-swift-emit-silgen %s -verify

// https://github.com/apple/swift/issues/55167

func thin(_: (@convention(thin) () -> Void) -> Void) {}
func block(_: (@convention(block) () -> Void) -> Void) {}
func c(_: (@convention(c) () -> Void) -> Void) {}

func function(_: () -> Void) {}

func context() {
    c(function)

    block(function)

    thin(function)
}

struct C {
    let function: (@convention(c) () -> Void) -> Void
}

struct Thin {
    let function: (@convention(thin) () -> Void) -> Void
}

struct Block {
    let function: (@convention(block) () -> Void) -> Void
}

func proxy(_ f: (() -> Void) -> Void) {
    let a = 1
    f { print(a) }
}

func cContext() {
    let c = C { app in app() }

    proxy(c.function)
    // expected-error@-1 {{converting non-escaping value to '@convention(c) () -> Void' may allow it to escape}}
    // expected-error@-2 {{cannot convert value of type '(@convention(c) () -> Void) -> Void' to expected argument type '(() -> Void) -> Void'}}

    let _ :  (@convention(block) () -> Void) -> Void = c.function 
    // expected-error@-1 {{converting non-escaping value to '@convention(c) () -> Void' may allow it to escape}}
    // expected-error@-2 {{cannot convert value of type '(@convention(c) () -> Void) -> Void' to specified type '(@convention(block) () -> Void) -> Void'}}

    let _ :  (@convention(c) () -> Void) -> Void = c.function // OK

    let _ :  (@convention(thin) () -> Void) -> Void = c.function // OK
    
    let _ :  (() -> Void) -> Void = c.function 
    // expected-error@-1 {{converting non-escaping value to '@convention(c) () -> Void' may allow it to escape}}
    // expected-error@-2 {{cannot convert value of type '(@convention(c) () -> Void) -> Void' to specified type '(() -> Void) -> Void'}}

}

func thinContext() {
    let thin = Thin { app in app() }

    proxy(thin.function) 
    // expected-error@-1 {{converting non-escaping value to '@convention(thin) () -> Void' may allow it to escape}}
    // expected-error@-2 {{cannot convert value of type '(@escaping @convention(thin) () -> Void) -> Void' to expected argument type '(() -> Void) -> Void'}}

    let _ :  (@convention(block) () -> Void) -> Void = thin.function 
    // expected-error@-1 {{converting non-escaping value to '@convention(thin) () -> Void' may allow it to escape}}
    // expected-error@-2 {{cannot convert value of type '(@escaping @convention(thin) () -> Void) -> Void' to specified type '(@convention(block) () -> Void) -> Void'}}

    let _ :  (@convention(c) () -> Void) -> Void = thin.function // OK 

    let _ :  (@convention(thin) () -> Void) -> Void = thin.function // OK

    let _ :  (() -> Void) -> Void = thin.function 
    // expected-error@-1 {{converting non-escaping value to '@convention(thin) () -> Void' may allow it to escape}}
    // expected-error@-2 {{cannot convert value of type '(@escaping @convention(thin) () -> Void) -> Void' to specified type '(() -> Void) -> Void'}}
}

func blockContext() {
    let block = Block { app in app() }

    proxy(block.function)

    let _ :  (@convention(block) () -> Void) -> Void = block.function // OK

    let _ :  (@convention(c) () -> Void) -> Void = block.function // OK

    let _ :  (@convention(thin) () -> Void) -> Void = block.function // OK

    let _ :  (() -> Void) -> Void = block.function  // OK
}
