// RUN: not %target-swift-frontend -typecheck -diagnostic-style=llvm -disable-objc-attr-requires-foundation-module %s -sdk %clang-importer-sdk -experimental-print-full-convention -use-clang-function-types 2>&1 | %FileCheck %s --implicit-check-not=error:

// This uses FileCheck rather than -verify because the C types in the messages
// depend on the target: Int is 'long' on most platforms but 'long long' on
// Windows. Every error must be matched by a CHECK line.

import ctypes

// Setting a C function type with the correct cType works.
let f1 : (@convention(c, cType: "size_t (*)(size_t)") (Int) -> Int)? = getFunctionPointer_()

// However, trying to convert between @convention(c) functions
// with differing cTypes doesn't work.

let _ : @convention(c) (Int) -> Int = f1!
// CHECK: :[[@LINE-1]]:{{[0-9]+}}: error: cannot convert function with underlying C type 'size_t (*)(size_t)' to C type '[[LONG:long( long)?]] (*)([[LONG]])'

let _ : (@convention(c) (Int) -> Int)? = f1
// CHECK: :[[@LINE-1]]:{{[0-9]+}}: error: cannot convert function with underlying C type 'size_t (*)(size_t)' to C type '[[LONG]] (*)([[LONG]])'

let _ : (@convention(c, cType: "void *(*)(void *)") (Int) -> Int)? = f1
// CHECK: :[[@LINE-1]]:{{[0-9]+}}: error: cannot convert function with underlying C type 'size_t (*)(size_t)' to C type 'void *(*)(void *)'

// We only use the special diagnostic when there are no other type mismatches.
let _ : @convention(c, cType: "void *(*)(void *)") (OpaquePointer?) -> Int = f1!
// CHECK: :[[@LINE-1]]:{{[0-9]+}}: error: cannot convert value of type '@convention(c, cType: "size_t (*)(size_t)") (Int) -> Int' to specified type '@convention(c, cType: "void *(*)(void *)") (OpaquePointer?) -> Int'


// Converting from @convention(c) -> @convention(swift) works

let _ : (Int) -> Int = ({ x in x } as @convention(c) (Int) -> Int)
let _ : (Int) -> Int = ({ x in x } as @convention(c, cType: "size_t (*)(size_t)") (Int) -> Int)


// Converting from @convention(swift) -> @convention(c) doesn't work.

let fs : (Int) -> Int = { x in x }

let _ : @convention(c) (Int) -> Int = fs
// CHECK: :[[@LINE-1]]:{{[0-9]+}}: error: a C function pointer can only be formed from a reference to a 'func' or a literal closure

let _ : @convention(c, cType: "size_t (*)(size_t)") (Int) -> Int = fs
// CHECK: :[[@LINE-1]]:{{[0-9]+}}: error: a C function pointer can only be formed from a reference to a 'func' or a literal closure


// More complex examples.

let f2 : (@convention(c) ((@convention(c, cType: "size_t (*)(size_t)") (Swift.Int) -> Swift.Int)?) -> (@convention(c, cType: "size_t (*)(size_t)") (Swift.Int) -> Swift.Int)?)? = getHigherOrderFunctionPointer()!

let _ : (@convention(c) ((@convention(c) (Swift.Int) -> Swift.Int)?) -> (@convention(c, cType: "size_t (*)(size_t)") (Swift.Int) -> Swift.Int)?)? = f2!
// CHECK: :[[@LINE-1]]:{{[0-9]+}}: error: cannot convert function with underlying C type 'unsigned [[LONG]] (*(*)(unsigned [[LONG]] (*)(unsigned [[LONG]])))(unsigned [[LONG]])' to C type 'unsigned [[LONG]] (*(*)([[LONG]] (*)([[LONG]])))(unsigned [[LONG]])'

let _ : (@convention(c) ((@convention(c) (Swift.Int) -> Swift.Int)?) -> (@convention(c) (Swift.Int) -> Swift.Int)?)? = f2!
// CHECK: :[[@LINE-1]]:{{[0-9]+}}: error: cannot convert function with underlying C type 'unsigned [[LONG]] (*(*)(unsigned [[LONG]] (*)(unsigned [[LONG]])))(unsigned [[LONG]])' to C type '[[LONG]] (*(*)([[LONG]] (*)([[LONG]])))([[LONG]])'

let f3 = getFunctionPointer3

let _ : @convention(c) (UnsafeMutablePointer<ctypes.Dummy>?) -> UnsafeMutablePointer<ctypes.Dummy>? = f3()!


// If there are several solutions that add the fix in different places, they're
// treated as identical, not incomparable.

func identity<T>(_ x: T) -> T { x }

let _ : @convention(c) (Int) -> Int = identity(f1!)
// CHECK: :[[@LINE-1]]:{{[0-9]+}}: error: cannot convert function with underlying C type 'size_t (*)(size_t)' to C type '[[LONG]] (*)([[LONG]])'

let _ : (@convention(c) (Int) -> Int)? = identity(f1)
// CHECK: :[[@LINE-1]]:{{[0-9]+}}: error: cannot convert function with underlying C type 'size_t (*)(size_t)' to C type '[[LONG]] (*)([[LONG]])'

let g1 : [@convention(c, cType: "size_t (*)(size_t)") (Int) -> Int] = [f1!]
let _ : [@convention(c) (Int) -> Int] = identity(g1)
// CHECK: :[[@LINE-1]]:{{[0-9]+}}: error: cannot convert function with underlying C type 'size_t (*)(size_t)' to C type '[[LONG]] (*)([[LONG]])'

let _ : @convention(c) (Int) -> Int = identity(identity(f1!))
// CHECK: :[[@LINE-1]]:{{[0-9]+}}: error: cannot convert function with underlying C type 'size_t (*)(size_t)' to C type '[[LONG]] (*)([[LONG]])'

let fs2 : @convention(c) (Int) -> Int = { $0 }
func useInTernary(_ cond: Bool) {
  let _ : @convention(c) (Int) -> Int = cond ? identity(f1!) : fs2
  // CHECK: :[[@LINE-1]]:{{[0-9]+}}: error: cannot convert function with underlying C type 'size_t (*)(size_t)' to C type '[[LONG]] (*)([[LONG]])'
}

