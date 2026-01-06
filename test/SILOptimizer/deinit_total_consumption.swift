// RUN: %target-swift-frontend -DEMPTY -emit-sil -verify %s
// RUN: %target-swift-frontend -DTRIVIAL -emit-sil -verify %s
// RUN: %target-swift-frontend -DLOADABLE -emit-sil -verify %s
// RUN: %target-swift-frontend -DADDRESS_ONLY -emit-sil -verify %s

// Don't allow whole-value consumption or mutation of a value in its own
// `deinit`, but allow consumption of its individual fields.

struct T1: ~Copyable {
#if EMPTY
#elseif TRIVIAL
    var _x: Int
#elseif LOADABLE
    var _x: AnyObject
#elseif ADDRESS_ONLY
    var _x: Any
#else
    #error("pick one")
#endif
    deinit {
        self.foo() // expected-error{{'self' cannot be consumed during 'deinit'}}
    }

    consuming func foo() {}
}

struct T2: ~Copyable {
#if EMPTY
#elseif TRIVIAL
    var _x: Int
#elseif LOADABLE
    var _x: AnyObject
#elseif ADDRESS_ONLY
    var _x: Any
#else
    #error("pick one")
#endif

    deinit {
        let myself = self // expected-error{{'self' cannot be consumed during 'deinit'}}
        _ = myself
    }
}

func consume<T: ~Copyable>(_: consuming T) {}

struct NC<T: ~Copyable>: ~Copyable {
    var x: T
}

struct T3: ~Copyable {
#if EMPTY
    var _x: NC<()>
#elseif TRIVIAL
    var _x: NC<Int>
#elseif LOADABLE
    var _x: NC<AnyObject>
#elseif ADDRESS_ONLY
    var _x: NC<Any>
#else
    #error("pick one")
#endif

    // consuming the single field of a one-field type is OK
    deinit {
        consume(_x)
    }
}

struct T4: ~Copyable {
#if EMPTY
#elseif TRIVIAL
    var _x: Int
#elseif LOADABLE
    var _x: AnyObject
#elseif ADDRESS_ONLY
    var _x: Any
#else
    #error("pick one")
#endif

    // the implicit cleanup of the value is OK
    deinit {
    }
}

struct T5: ~Copyable {
#if EMPTY
    var _x: NC<()>
#elseif TRIVIAL
    var _x: NC<Int>
#elseif LOADABLE
    var _x: NC<AnyObject>
#elseif ADDRESS_ONLY
    var _x: NC<Any>
#else
    #error("pick one")
#endif

    // the implicit cleanup of the value is OK
    deinit {
    }
}

