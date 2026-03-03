// RUN: %target-typecheck-verify-swift

enum Foo: ~Copyable {
    deinit {} // expected-error {{deinitializers are not yet supported on noncopyable enums}}
}

enum Foo2: ~Copyable {
}

struct Bar: ~Copyable {
    var x: Int
}

enum Foo3: ~Copyable {
    case foo(Int)
    case bar(String)
    case bas
}

func test_switch(x: consuming Foo3) {
    switch x { 
    default:
        break
    }

    switch consume x {
    default:
        break
    }

    switch (x) { 
    default:
        break
    }

    switch (consume x) {
    default:
        break
    }

    let _: () -> () = {
        switch x { 
        default:
            break
        }
    }

    let _: () -> () = {
        switch consume x {
        default:
            break
        }
    }

    let _: () -> () = {
        switch (x) { 
        default:
            break
        }
    }

    let _: () -> () = {
        switch (consume x) {
        default:
            break
        }
    }
}

func test_if_case(x: consuming Foo3) {
    if case .bar(let y) = x { _ = y } 

    guard case .bar(let y) = x else { return } 
    _ = y

    if case .bar(let z) = consume x { _ = z }

    guard case .bar(let z) = consume x else { return }
    _ = z

    if case .bar(let a) = (x) { _ = a } 

    guard case .bar(let a) = (x) else { return } 
    _ = a

    if case .bar(let b) = (consume x) { _ = b }

    guard case .bar(let b) = (consume x) else { return }
    _ = b

    let _: () -> () = {
        if case .bar(let y) = x { _ = y } 
    }

    let _: () -> () = {
        guard case .bar(let y) = x else { return } 
        _ = y
    }

    let _: () -> () = {
        if case .bar(let z) = consume x { _ = z }
    }

    let _: () -> () = {
        guard case .bar(let z) = consume x else { return }
        _ = z
    }

    let _: () -> () = {
        if case .bar(let a) = (x) { _ = a } 
    }

    let _: () -> () = {
        guard case .bar(let a) = (x) else { return } 
        _ = a
    }

    let _: () -> () = {
        if case .bar(let b) = (consume x) { _ = b }
    }

    let _: () -> () = {
        guard case .bar(let b) = (consume x) else { return }
        _ = b
    }
}

func test_switch_b(x: __owned Foo3) {
    switch x { 
    default:
        break
    }

    switch consume x {
    default:
        break
    }

    switch (x) { 
    default:
        break
    }

    switch (consume x) {
    default:
        break
    }

    let _: () -> () = {
        switch x { 
        default:
            break
        }
    }

    let _: () -> () = {
        switch consume x {
        default:
            break
        }
    }

    let _: () -> () = {
        switch (x) { 
        default:
            break
        }
    }

    let _: () -> () = {
        switch (consume x) {
        default:
            break
        }
    }
}

func test_if_case_b(x: __owned Foo3) {
    if case .bar(let y) = x { _ = y } 

    guard case .bar(let y) = x else { return } 
    _ = y

    if case .bar(let z) = consume x { _ = z }

    guard case .bar(let z) = consume x else { return }
    _ = z

    if case .bar(let a) = (x) { _ = a } 

    guard case .bar(let a) = (x) else { return } 
    _ = a

    if case .bar(let b) = (consume x) { _ = b }

    guard case .bar(let b) = (consume x) else { return }
    _ = b

    let _: () -> () = {
        if case .bar(let y) = x { _ = y } 
    }

    let _: () -> () = {
        guard case .bar(let y) = x else { return } 
        _ = y
    }

    let _: () -> () = {
        if case .bar(let z) = consume x { _ = z }
    }

    let _: () -> () = {
        guard case .bar(let z) = consume x else { return }
        _ = z
    }

    let _: () -> () = {
        if case .bar(let a) = (x) { _ = a } 
    }

    let _: () -> () = {
        guard case .bar(let a) = (x) else { return } 
        _ = a
    }

    let _: () -> () = {
        if case .bar(let b) = (consume x) { _ = b }
    }

    let _: () -> () = {
        guard case .bar(let b) = (consume x) else { return }
        _ = b
    }
}
