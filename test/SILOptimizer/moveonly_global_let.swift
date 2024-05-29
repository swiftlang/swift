// RUN: %target-swift-frontend -parse-as-library -DADDRESS_ONLY -emit-sil -verify %s
// RUN: %target-swift-frontend -parse-as-library -DLOADABLE -emit-sil -verify %s
// RUN: %target-swift-frontend -parse-as-library -DTRIVIAL -emit-sil -verify %s
// RUN: %target-swift-frontend -parse-as-library -DEMPTY -emit-sil -verify %s

// RUN: %target-swift-frontend -DADDRESS_ONLY -emit-sil -verify %s
// RUN: %target-swift-frontend -DLOADABLE -emit-sil -verify %s
// RUN: %target-swift-frontend -DTRIVIAL -emit-sil -verify %s
// RUN: %target-swift-frontend -DEMPTY -emit-sil -verify %s

struct Butt: ~Copyable {
#if ADDRESS_ONLY
    var x: Any
#elseif LOADABLE
    var x: AnyObject
#elseif TRIVIAL
    var x: Int
#elseif EMPTY
#else
  #error("pick one")
#endif

    init() { fatalError() }

    borrowing func method() {}
}

func freefunc(_: borrowing Butt) {}

let global = Butt()

struct StaticHolder {
    static let staticMember = Butt()
}

func foo() {
    freefunc(global)
    freefunc(StaticHolder.staticMember)
    global.method()
    StaticHolder.staticMember.method()
}

func consume(_: consuming Butt) {}

func tryConsume() {
    // FIXME: gives different diagnostics for parse-as-library vs script global
    consume(global) // expected-error{{}} expected-note *{{}}
    consume(StaticHolder.staticMember) // expected-error{{cannot be consumed}} expected-note{{consumed here}}
}

var globalVar = Butt()

func mutate(_: inout Butt) {}

func manipulateGlobalVar() {
    freefunc(globalVar)
    mutate(&globalVar)
    // FIXME: gives different diagnostics for parse-as-library vs script global
    consume(globalVar) // expected-error{{consume}}
}
