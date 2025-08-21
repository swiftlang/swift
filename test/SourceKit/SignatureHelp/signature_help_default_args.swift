func add(_ x: Int = 10, to y: Int) -> Int {}

func add(oneTo x: inout Int) {}

func add(_ x: Int, to y: Int? = nil) -> String {}

func add(first: Double!, second: Float = .pi, third: Int) -> Double {}

struct S {
  let a: Bool
}

func add(s: S = S(a: false)) -> Double {}

func add(x: Int, y: Int, with adder: (Int, Int) -> Int = { $0 + $1 }) -> Int {}

let importantValue = 42

func add(x: Int = importantValue) {}

func add(x: Int, line: UInt = #line, file: StaticString = #file) {}

add()

// RUN: %sourcekitd-test -req=signaturehelp -pos=23:5 %s -- %s | %FileCheck -check-prefix=CHECK %s

// CHECK:      {
// CHECK-NEXT:   key.signatures: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "add(_ x: Int = 10, to: Int) -> Int",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 13
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 19,
// CHECK-NEXT:           key.namelength: 7
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "add(oneTo: inout Int)",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 16
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "add(_ x: Int, to: Int? = nil) -> String",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 8
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 14,
// CHECK-NEXT:           key.namelength: 14
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "add(first: Double!, second: Float = .pi, third: Int) -> Double",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 14
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 20,
// CHECK-NEXT:           key.namelength: 19
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 41,
// CHECK-NEXT:           key.namelength: 10
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "add(s: S = S(a: false)) -> Double",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 18
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "add(x: Int, y: Int, with: (Int, Int) -> Int = { $0 + $1 }) -> Int",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 6
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 12,
// CHECK-NEXT:           key.namelength: 6
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 20,
// CHECK-NEXT:           key.namelength: 37
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "add(x: Int = importantValue)",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 23
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "add(x: Int, line: UInt = #line, file: StaticString = #file)",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 6
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 12,
// CHECK-NEXT:           key.namelength: 18
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 32,
// CHECK-NEXT:           key.namelength: 26
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   key.active_signature: 0
// CHECK-NEXT: }
