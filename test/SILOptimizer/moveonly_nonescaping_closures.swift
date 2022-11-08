// RUN: %target-swift-frontend -enable-experimental-move-only -emit-sil -verify %s
// TODO: test with (-DNONTRIVIAL | -DADDRESS_ONLY) * (REABSTRACT)

protocol P {}
extension Int: P {}

class C {}

@_moveOnly
struct M {
#if ADDRESS_ONLY
    private var x: P = 0
#elseif NONTRIVIAL
    private var x: C = C()
#else
    private var x: Int = 0
#endif
}

func borrow(_: __shared M) {}
func consume(_: __owned M) {}
func mutate(_: inout M) {}

func borrow(_: __shared M, consume _: __owned M) {}

#if REABSTRACT
func clodger<T>(_: () -> T) {}
func clodger<T>(_: () -> T, borrow _: __shared M) {}
func clodger<T>(_: () -> T, consume _: __owned M) {}
func clodger<T>(_: () -> T, mutate _: inout M) {}
func clodger<T>(_: () -> T, and _: () -> T) {}
func clodger<T>(_: () -> T, and _: () -> T, consume _: __owned M) {}
#else
func clodger(_: () -> ()) {}
func clodger(_: () -> (), borrow _: __shared M) {}
func clodger(_: () -> (), consume _: __owned M) {}
func clodger(_: () -> (), mutate _: inout M) {}
func clodger(_: () -> (), and _: () -> ()) {}
func clodger(_: () -> (), and _: () -> (), consume _: __owned M) {}
#endif

func reabstractClodger<T>(_: (T) -> T) {}

func a(x: __shared M) {
    clodger({ borrow(x) })
    borrow(x)
}

func b(x: __owned M) { // expected-error{{}}
    clodger({ borrow(x) }, consume: x) // expected-note{{}}
}

func c(x: __owned M) {
    clodger({ borrow(x) })
    borrow(x)
    consume(x)
}

func d(x: __owned M) { // expected-error{{}}
    clodger({ consume(x) }) // expected-note{{}}
}

func e(x: inout M) {
    clodger({ mutate(&x) })
    mutate(&x)
    borrow(x)
    consume(x)
    x = M()
}

func f(x: __shared M) {
    clodger({ borrow(x) }, borrow: x)
}

// TODO: Interfering inout/borrow access
func g(x: inout M) { // e/xpected-error{{}}
    clodger({ mutate(&x) }, borrow: x) // e/xpected-note{{}}
}

// TODO: Interfering inout/borrow access
func h(x: inout M) { // e/xpected-error{{}}
    clodger({ mutate(&x) }, consume: x) // e/xpected-note{{}}
    x = M()
}

// TODO: Interfering inout/borrow access
func i(x: inout M) { // e/xpected-error{{}}
    clodger({ mutate(&x) }, mutate: &x) // e/xpected-note{{}}
}

// Multiple closures are allowed to capture the same inout binding concurrently.
// Restrictions in the callee prevent them from being executed simultaneously.
func j(x: inout M) {
    clodger({ mutate(&x) }, and: { mutate(&x) })
}

func k(x: __shared M) {
    clodger({ borrow(x) }, and: { borrow(x) })
    borrow(x)
}


func l(x: inout M) { // expected-error{{}}
    clodger({ consume(x) }) // expected-note{{}}
}

func m(x: inout M) { // expected-error{{}}
    consume(x) // expected-note{{}}

    clodger({ borrow(x) }) // expected-note{{}}
}

func n(x: inout M) { // expected-error{{}}
    consume(x) // expected-note{{}}

    clodger({ // expected-note{{}}
        mutate(&x)
    })
}

func o(x: inout M) {
    clodger({ consume(x); x = M() })
}

func p(x: inout M) {
    consume(x)
    x = M()

    clodger({ consume(x); x = M() })
}

// need test cases for:
// - capturing local let
// - capturing local var
// - capture list binding can't be consumed
// - andy's bookmarked test cases
// - nested closure captures
