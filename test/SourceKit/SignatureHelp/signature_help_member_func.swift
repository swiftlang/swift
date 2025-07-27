struct Adder {
  func add(_ x: Int, to y: Int) -> Int {
    return x + y
  }
  
  func add(oneTo x: inout Int) {
    x += 1
  }
  
  func add<T: AdditiveArithmetic>(_ x: T, to y: T) -> T {
    return x + y
  }
  
  func add(first: Double, second: Float, third: Int) -> Double {
    return first + second + third
  }
  
  func add(arg1 param1: Double, arg2: Float, arg3 param3: Int) -> Double {
    return param1 + arg2 + param3
  }
  
  func add(numbers: Double...) -> Double {
    return numbers.reduce(into: 0, +)
  }
  
  func add(x: Int, y: Int, with adder: (Int, Int) -> Int) -> Int {
    return adder(x, y)
  }
  
  func add(x: Int) -> (Int) -> Int {
    return { (y: Int) in x + y }
  }
}

let adder = Adder()
adder.add()

// RUN: %sourcekitd-test -req=signaturehelp -pos=36:11 %s -- %s | %FileCheck -check-prefix=CHECK %s

// CHECK:      {
// CHECK:        key.members: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "func add(_ x: Int, to: Int) -> Int",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 9,
// CHECK-NEXT:           key.namelength: 8
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 19,
// CHECK-NEXT:           key.namelength: 7
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "func add(oneTo: inout Int)",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 9,
// CHECK-NEXT:           key.namelength: 16
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "func add(_ x: T, to: T) -> T",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 9,
// CHECK-NEXT:           key.namelength: 6
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 17,
// CHECK-NEXT:           key.namelength: 5
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "func add(first: Double, second: Float, third: Int) -> Double",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 9,
// CHECK-NEXT:           key.namelength: 13
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 24,
// CHECK-NEXT:           key.namelength: 13
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 39,
// CHECK-NEXT:           key.namelength: 10
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "func add(arg1: Double, arg2: Float, arg3: Int) -> Double",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 9,
// CHECK-NEXT:           key.namelength: 12
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 23,
// CHECK-NEXT:           key.namelength: 11
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 36,
// CHECK-NEXT:           key.namelength: 9
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "func add(numbers: Double...) -> Double",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 9,
// CHECK-NEXT:           key.namelength: 18
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "func add(x: Int, y: Int, with: (Int, Int) -> Int) -> Int",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 9,
// CHECK-NEXT:           key.namelength: 6
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 17,
// CHECK-NEXT:           key.namelength: 6
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 25,
// CHECK-NEXT:           key.namelength: 23
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "func add(x: Int) -> (Int) -> Int",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 9,
// CHECK-NEXT:           key.namelength: 6
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     }
// CHECK-NEXT:   ]
// CHECK-NEXT: }
