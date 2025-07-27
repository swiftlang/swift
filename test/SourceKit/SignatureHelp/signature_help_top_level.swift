func add(_ x: Int, to y: Int) -> Int {
  return x + y
}

func add(oneTo x: inout Int) {
  x += 1
}

func add<T: AdditiveArithmetic>(_ x: T, to y: T) -> T {
  return x + y
}

func add(first: Double!, second: Float, third: Int) -> Double {
  return first + Double(second) + Double(third)
}

func add(arg1 param1: Double, arg2: Float, arg3 param3: Int) -> Double {
  return param1 + Double(arg2) + Double(param3)
}

func add(numbers: Double...) -> Double {
  return numbers.reduce(into: 0) { $0 += $1 }
}

func add(x: Int, y: Int, with adder: (Int, Int) -> Int) -> Int {
  return adder(x, y)
}

func add(x: Int) -> (Int) -> Int {
  return { (y: Int) in x + y }
}

add()

// RUN: %sourcekitd-test -req=signaturehelp -pos=33:5 %s -- %s | %FileCheck -check-prefix=CHECK %s

// CHECK:      {
// CHECK-NEXT:   key.active_signature: 0,
// CHECK-NEXT:   key.members: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "add(_ x: Int, to: Int) -> Int",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 8
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 14,
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
// CHECK-NEXT:       key.name: "add(_ x: AdditiveArithmetic, to: AdditiveArithmetic) -> AdditiveArithmetic",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 23
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 29,
// CHECK-NEXT:           key.namelength: 22
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "add(first: Double!, second: Float, third: Int) -> Double",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 14
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 20,
// CHECK-NEXT:           key.namelength: 13
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 35,
// CHECK-NEXT:           key.namelength: 10
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "add(arg1: Double, arg2: Float, arg3: Int) -> Double",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 12
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 18,
// CHECK-NEXT:           key.namelength: 11
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 31,
// CHECK-NEXT:           key.namelength: 9
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "add(numbers: Double...) -> Double",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 18
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "add(x: Int, y: Int, with: (Int, Int) -> Int) -> Int",
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
// CHECK-NEXT:           key.namelength: 23
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "add(x: Int) -> (Int) -> Int",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 6
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     }
// CHECK-NEXT:   ]
// CHECK-NEXT: }
