// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=default
import InheritedLookup

extension One {
    // Swift extensions of a base class should not affect its derived classes.
    // We later attempt to call baseExt() in derived classes, which should fail.
    func baseExt() -> Int32 { return 0 }

    func ext() {
        let _ = baseExt()
        let _ = self[0]
        let _ = method()
        let _ = methodI()   // expected-error {{cannot find 'methodI' in scope}}
        let _ = methodII()  // expected-error {{cannot find 'methodII' in scope}}
        let _ = methodIII() // expected-error {{cannot find 'methodIII' in scope}}
    }
}

func fOne(v: One) {
    let _ = v.baseExt()
    let _ = v[0]
    let _ = v.method()
    let _ = v.methodI()     // expected-error {{'One' has no member 'methodI'}}
    let _ = v.methodII()    // expected-error {{'One' has no member 'methodII'}}
    let _ = v.methodIII()   // expected-error {{'One' has no member 'methodIII'}}
}

extension IOne {
    func ext() {
        let _ = baseExt()   // expected-error {{cannot find 'baseExt' in scope}}
        let _ = self[0]
        let _ = method()
        let _ = methodI()
        let _ = methodII()  // expected-error {{cannot find 'methodII' in scope}}
        let _ = methodIII() // expected-error {{cannot find 'methodIII' in scope}}
    }
}

func fIOne(v: IOne) {
    let _ = v.baseExt()     // expected-error {{'IOne' has no member 'baseExt'}}
    let _ = v[0]
    let _ = v.method()
    let _ = v.methodI()
    let _ = v.methodII()    // expected-error {{'IOne' has no member 'methodII'}}
    let _ = v.methodIII()   // expected-error {{'IOne' has no member 'methodIII'}}
}

extension IIOne {
    func ext() {
        let _ = baseExt()   // expected-error {{cannot find 'baseExt' in scope}}
        let _ = self[0]
        let _ = method()
        let _ = methodI()
        let _ = methodII()
        let _ = methodIII() // expected-error {{cannot find 'methodIII' in scope}}
    }
}

func fIIOne(v: IIOne) {
    let _ = v.baseExt()     // expected-error {{'IIOne' has no member 'baseExt'}}
    let _ = v[0]
    let _ = v.method()
    let _ = v.methodI()
    let _ = v.methodII()
    let _ = v.methodIII()   // expected-error {{'IIOne' has no member 'methodIII'}}
}

extension IIIOne {
    func ext() {
        let _ = baseExt()   // expected-error {{cannot find 'baseExt' in scope}}
        let _ = self[0]
        let _ = method()
        let _ = methodI()
        let _ = methodII()
        let _ = methodIII()
    }
}

func fIIIOne(v: IIIOne) {
    let _ = v.baseExt()     // expected-error {{'IIIOne' has no member 'baseExt'}}
    let _ = v[0]
    let _ = v.method()
    let _ = v.methodI()
    let _ = v.methodII()
    let _ = v.methodIII()
}
