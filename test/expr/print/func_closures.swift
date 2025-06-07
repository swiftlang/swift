// RUN: %target-swift-frontend -print-ast -disable-availability-checking %s 2>&1 | %FileCheck %s

func fetch() async throws -> String {
}
// CHECK: internal func fetch() async throws -> String {
// CHECK: }

let fn = { (x: Int) in
}
// CHECK: @_hasInitialValue internal let fn: (_ x: Int) -> () = { (x: Int) in
// CHECK: }

let fn1 = {}
// CHECK: @_hasInitialValue internal let fn1: () -> () = { () in
// CHECK: }

let fn2: (Int) -> Void = { x in }
// CHECK: @_hasInitialValue internal let fn2: (Int) -> Void = { (x: Int) in
// CHECK: }

let fn3: (Int, Int) -> Void = { x, y in }
// CHECK: @_hasInitialValue internal let fn3: (Int, Int) -> Void = { (x: Int, y: Int) in
// CHECK: }

let fn4: (Int, Int) -> Void = { (x, y) in }
// CHECK: @_hasInitialValue internal let fn4: (Int, Int) -> Void = { (x: Int, y: Int) in
// CHECK: }

let fn5 = { (x: String, y: Int) in }
// CHECK: @_hasInitialValue internal let fn5: (_ x: String, _ y: Int) -> () = { (x: String, y: Int) in
// CHECK: }

let fn6: (Int) -> Int = { x -> Int in x }
// CHECK: @_hasInitialValue internal let fn6: (Int) -> Int = { (x: Int) -> Int in
// CHECK:   return x
// CHECK: }

let fn7: (Int, Int) -> Int = { (x, y) -> Int in x + y }
// CHECK: @_hasInitialValue internal let fn7: (Int, Int) -> Int = { (x: Int, y: Int) -> Int in
// CHECK:   return x + y
// CHECK: }

let fn8 = { (x: Int, y: Int) -> Int in x + y }
// CHECK: @_hasInitialValue internal let fn8: (_ x: Int, _ y: Int) -> Int = { (x: Int, y: Int) -> Int in
// CHECK:   return x + y
// CHECK: }

let fn9 = { () -> Int in 0 }
// CHECK: @_hasInitialValue internal let fn9: () -> Int = { () -> Int in
// CHECK:   return 0
// CHECK: }

let fn10 = { () in }
// CHECK: @_hasInitialValue internal let fn10: () -> () = { () in
// CHECK: }
