// RUN: %target-swift-emit-silgen %s -verify

func SR12723_thin(_: (@convention(thin) () -> Void) -> Void) {}
func SR12723_block(_: (@convention(block) () -> Void) -> Void) {}
func SR12723_c(_: (@convention(c) () -> Void) -> Void) {}

func SR12723_function(_: () -> Void) {}

func context() {
    SR12723_c(SR12723_function) 

    SR12723_block(SR12723_function) 

    SR12723_thin(SR12723_function) 
}

struct SR12723_C {
    let function: (@convention(c) () -> Void) -> Void
}

struct SR12723_Thin {
    let function: (@convention(thin) () -> Void) -> Void
}

struct SR12723_Block {
    let function: (@convention(block) () -> Void) -> Void
}

func proxy(_ f: (() -> Void) -> Void) {
    let a = 1
    f { print(a) }
}

func cContext() {
    let c = SR12723_C { app in app() }

    proxy(c.function)
    // expected-error@-1 {{cannot convert value of type '(@convention(c) () -> Void) -> Void' to expected argument type '(() -> Void) -> Void'}}

    let _ :  (@convention(block) () -> Void) -> Void = c.function 
    // expected-error@-1 {{cannot convert value of type '(@convention(c) () -> Void) -> Void' to specified type '(@convention(block) () -> Void) -> Void'}}

    let _ :  (@convention(c) () -> Void) -> Void = c.function // OK

    let _ :  (@convention(thin) () -> Void) -> Void = c.function // OK
    
    let _ :  (() -> Void) -> Void = c.function 
    // expected-error@-1 {{cannot convert value of type '(@convention(c) () -> Void) -> Void' to specified type '(() -> Void) -> Void'}}

}

func thinContext() {
    let thin = SR12723_Thin { app in app() }

    proxy(thin.function) 
    // expected-error@-1 {{cannot convert value of type '(@convention(thin) () -> Void) -> Void' to expected argument type '(() -> Void) -> Void'}}

    let _ :  (@convention(block) () -> Void) -> Void = thin.function 
    // expected-error@-1 {{cannot convert value of type '(@convention(thin) () -> Void) -> Void' to specified type '(@convention(block) () -> Void) -> Void'}}

    let _ :  (@convention(c) () -> Void) -> Void = thin.function // OK 

    let _ :  (@convention(thin) () -> Void) -> Void = thin.function // OK

    let _ :  (() -> Void) -> Void = thin.function 
    // expected-error@-1 {{cannot convert value of type '(@convention(thin) () -> Void) -> Void' to specified type '(() -> Void) -> Void'}}
}

func blockContext() {
    let block = SR12723_Block { app in app() }

    proxy(block.function)

    let _ :  (@convention(block) () -> Void) -> Void = block.function // OK

    let _ :  (@convention(c) () -> Void) -> Void = block.function // OK

    let _ :  (@convention(thin) () -> Void) -> Void = block.function // OK

    let _ :  (() -> Void) -> Void = block.function  // OK
}
