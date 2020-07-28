// RUN: %target-swift-frontend -emit-sil %s -verify -Osize
//
// NOTE: We only emit opt-remarks with -Osize,-O today! -O does drop way more
// stuff though, so we test with -Osize.

public class Klass {}

public var mySingleton = Klass()

@_semantics("optremark")
@inline(never)
public func forceOptRemark() -> Klass {
    return mySingleton // expected-remark {{retain}}
                       // expected-note @-6 {{of 'mySingleton'}}
}

@_semantics("optremark.sil-opt-remark-gen")
@inline(never)
public func forceOptRemark2() -> Klass {
    return mySingleton // expected-remark {{retain}}
                       // expected-note @-13 {{of 'mySingleton'}}
}

@_semantics("optremark.fail")
@inline(never)
public func failMatch() -> Klass {
    return mySingleton
}

@_semantics("optremark")
public func allocateInlineCallee() -> Klass {
    return Klass() // expected-remark {{Pure call. Always profitable to inline "main.Klass.__allocating_init()"}}
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
    return (mySingleton, Klass()) // expected-remark {{Pure call. Always profitable to inline "main.Klass.__allocating_init()"}}
                                  // expected-remark @-1:5 {{retain}}
                                  // expected-note @-42:12 {{of 'mySingleton'}}
}

@_semantics("optremark.sil-inliner")
public func mix2() -> (Klass, Klass) {
    return (mySingleton, Klass()) // expected-remark {{Pure call. Always profitable to inline "main.Klass.__allocating_init()"}}
}

@_semantics("optremark.sil-opt-remark-gen")
public func mix3() -> (Klass, Klass) {
    return (mySingleton, Klass()) // expected-remark @:5 {{retain}}
                                  // expected-note @-53:12 {{of 'mySingleton'}}
}

@_semantics("optremark")
public func mix4() -> (Klass, Klass) {
    return (mySingleton, Klass()) // expected-remark {{Pure call. Always profitable to inline "main.Klass.__allocating_init()"}}
                                  // expected-remark @-1:5 {{retain}}
                                  // expected-note @-60:12 {{of 'mySingleton'}}
}

public func mix5() -> (Klass, Klass) {
    return (mySingleton, Klass())
}
