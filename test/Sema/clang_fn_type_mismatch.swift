// RUN: %target-typecheck-verify-swift -sdk %clang-importer-sdk -experimental-print-full-convention -use-clang-function-types

import ctypes

// Setting a C function type with the correct cType works.
let f1 : (@convention(c, cType: "size_t (*)(size_t)") (Int) -> Int)? = getFunctionPointer_()

// However, trying to convert between @convention(c) functions
// with differing cTypes doesn't work.

let _ : @convention(c) (Int) -> Int = f1!
// expected-error@-1{{cannot convert value of type '@convention(c, cType: "size_t (*)(size_t)") (Int) -> Int' to specified type '@convention(c) (Int) -> Int'}}

let _ : (@convention(c) (Int) -> Int)? = f1
// expected-error@-1{{cannot assign value of type '(@convention(c, cType: "size_t (*)(size_t)") (Int) -> Int)?' to type '(@convention(c) (Int) -> Int)?'}}
// expected-note@-2 {{arguments to generic parameter 'Wrapped' ('@convention(c, cType: "size_t (*)(size_t)") (Int) -> Int' and '@convention(c) (Int) -> Int') are expected to be equal}}

let _ : (@convention(c, cType: "void *(*)(void *)") (Int) -> Int)? = f1
// expected-error@-1{{cannot assign value of type '(@convention(c, cType: "size_t (*)(size_t)") (Int) -> Int)?' to type '(@convention(c, cType: "void *(*)(void *)") (Int) -> Int)?'}}
// expected-note@-2 {{arguments to generic parameter 'Wrapped' ('@convention(c, cType: "size_t (*)(size_t)") (Int) -> Int' and '@convention(c, cType: "void *(*)(void *)") (Int) -> Int') are expected to be equal}}


// Converting from @convention(c) -> @convention(swift) works

let _ : (Int) -> Int = ({ x in x } as @convention(c) (Int) -> Int)
let _ : (Int) -> Int = ({ x in x } as @convention(c, cType: "size_t (*)(size_t)") (Int) -> Int)


// Converting from @convention(swift) -> @convention(c) doesn't work.

let fs : (Int) -> Int = { x in x }

let _ : @convention(c) (Int) -> Int = fs
// expected-error@-1{{a C function pointer can only be formed from a reference to a 'func' or a literal closure}}

let _ : @convention(c, cType: "size_t (*)(size_t)") (Int) -> Int = fs
// expected-error@-1{{a C function pointer can only be formed from a reference to a 'func' or a literal closure}}


// More complex examples.

let f2 : (@convention(c) ((@convention(c, cType: "size_t (*)(size_t)") (Swift.Int) -> Swift.Int)?) -> (@convention(c, cType: "size_t (*)(size_t)") (Swift.Int) -> Swift.Int)?)? = getHigherOrderFunctionPointer()!

let _ : (@convention(c) ((@convention(c) (Swift.Int) -> Swift.Int)?) -> (@convention(c, cType: "size_t (*)(size_t)") (Swift.Int) -> Swift.Int)?)? = f2!
// expected-error@-1{{cannot convert value of type '@convention(c) ((@convention(c, cType: "size_t (*)(size_t)") (Int) -> Int)?) -> (@convention(c, cType: "size_t (*)(size_t)") (Int) -> Int)?' to specified type '@convention(c) ((@convention(c) (Int) -> Int)?) -> (@convention(c, cType: "size_t (*)(size_t)") (Int) -> Int)?'}}

let _ : (@convention(c) ((@convention(c) (Swift.Int) -> Swift.Int)?) -> (@convention(c) (Swift.Int) -> Swift.Int)?)? = f2!
// expected-error@-1{{cannot convert value of type '@convention(c) ((@convention(c, cType: "size_t (*)(size_t)") (Int) -> Int)?) -> (@convention(c, cType: "size_t (*)(size_t)") (Int) -> Int)?' to specified type '@convention(c) ((@convention(c) (Int) -> Int)?) -> (@convention(c) (Int) -> Int)?'}}

let f3 = getFunctionPointer3

let _ : @convention(c) (UnsafeMutablePointer<ctypes.Dummy>?) -> UnsafeMutablePointer<ctypes.Dummy>? = f3()!
