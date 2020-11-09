// RUN: %target-swift-frontend -emit-sil %s -verify -Osize -o /dev/null -module-name main
//
// NOTE: We only emit opt-remarks with -Osize,-O today! -O does drop way more
// stuff though, so we test with -Osize.

public class Klass {}

public var mySingleton = Klass()

@inline(never)
func getGlobal() -> Klass {
    return mySingleton
}

@inline(never)
func useKlass(_ k: Klass) {}

@_semantics("optremark")
@inline(never)
public func forceOptRemark() {
    let x = getGlobal()
    useKlass(x) // expected-remark {{release of type 'Klass'}}
                // expected-note @-2 {{of 'x'}}
}

@_semantics("optremark.sil-opt-remark-gen")
@inline(never)
public func forceOptRemark2() {
    let x = getGlobal()
    useKlass(x) // expected-remark {{release of type 'Klass'}}
                // expected-note @-2 {{of 'x'}}
}

@_semantics("optremark.fail")
@inline(never)
public func failMatch() {
    let x = getGlobal()
    useKlass(x)
}

@_semantics("optremark")
public func allocateInlineCallee() -> Klass {
    return Klass() // expected-remark {{Pure call. Always profitable to inline "main.Klass.__allocating_init()"}}
                   // expected-remark @-1 {{heap allocated ref of type 'Klass'}}
}

@_semantics("optremark.sil-inliner")
public func allocateInlineCallee2() -> Klass {
    return Klass() // expected-remark {{Pure call. Always profitable to inline "main.Klass.__allocating_init()"}}
}

// This makes sure we don't emit any remarks if we do not have semantics.
public func allocateInlineCallee3() -> Klass {
    return Klass()
}

@_semantics("optremark.sil-inliner")
@_semantics("optremark.sil-opt-remark-gen")
public func mix1() -> (Klass, Klass) {
    let x = getGlobal()
    return (x, Klass()) // expected-remark {{Pure call. Always profitable to inline "main.Klass.__allocating_init()"}}
                        // expected-remark @-1:16 {{heap allocated ref of type 'Klass'}}
}

@_semantics("optremark.sil-inliner")
public func mix2() -> (Klass, Klass) {
    let x = getGlobal()
    return (x, Klass()) // expected-remark {{Pure call. Always profitable to inline "main.Klass.__allocating_init()"}}
}

@_semantics("optremark.sil-opt-remark-gen")
public func mix3() -> (Klass, Klass) {
    let x = getGlobal()
    return (x, Klass()) // expected-remark {{heap allocated ref of type 'Klass'}}
}

@_semantics("optremark")
public func mix4() -> (Klass, Klass) {
    let x = getGlobal()
    return (x, Klass()) // expected-remark {{Pure call. Always profitable to inline "main.Klass.__allocating_init()"}}
                        // expected-remark @-1 {{heap allocated ref of type 'Klass'}}
}

public func mix5() -> (Klass, Klass) {
    let x = getGlobal()
    return (x, Klass())
}
