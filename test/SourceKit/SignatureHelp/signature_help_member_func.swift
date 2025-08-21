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
}

let adder = Adder()
adder.add()

// RUN: %sourcekitd-test -req=signaturehelp -pos=36:11 %s -- %s | %FileCheck -check-prefix=MEMBER %s

// MEMBER:      {
// MEMBER-NEXT:   key.signatures: [
// MEMBER-NEXT:     {
// MEMBER-NEXT:       key.name: "add(_ x: Int, to: Int) -> Int",
// MEMBER-NEXT:       key.parameters: [
// MEMBER-NEXT:         {
// MEMBER-NEXT:           key.nameoffset: 4,
// MEMBER-NEXT:           key.namelength: 8
// MEMBER-NEXT:         },
// MEMBER-NEXT:         {
// MEMBER-NEXT:           key.nameoffset: 14,
// MEMBER-NEXT:           key.namelength: 7
// MEMBER-NEXT:         }
// MEMBER-NEXT:       ],
// MEMBER-NEXT:       key.active_parameter: 0
// MEMBER-NEXT:     },
// MEMBER-NEXT:     {
// MEMBER-NEXT:       key.name: "add(oneTo: inout Int)",
// MEMBER-NEXT:       key.parameters: [
// MEMBER-NEXT:         {
// MEMBER-NEXT:           key.nameoffset: 4,
// MEMBER-NEXT:           key.namelength: 16
// MEMBER-NEXT:         }
// MEMBER-NEXT:       ],
// MEMBER-NEXT:       key.active_parameter: 0
// MEMBER-NEXT:     },
// MEMBER-NEXT:     {
// MEMBER-NEXT:       key.name: "add(_ x: AdditiveArithmetic, to: AdditiveArithmetic) -> AdditiveArithmetic",
// MEMBER-NEXT:       key.parameters: [
// MEMBER-NEXT:         {
// MEMBER-NEXT:           key.nameoffset: 4,
// MEMBER-NEXT:           key.namelength: 23
// MEMBER-NEXT:         },
// MEMBER-NEXT:         {
// MEMBER-NEXT:           key.nameoffset: 29,
// MEMBER-NEXT:           key.namelength: 22
// MEMBER-NEXT:         }
// MEMBER-NEXT:       ],
// MEMBER-NEXT:       key.active_parameter: 0
// MEMBER-NEXT:     },
// MEMBER-NEXT:     {
// MEMBER-NEXT:       key.name: "add(first: Double!, second: Float, third: Int) -> Double",
// MEMBER-NEXT:       key.parameters: [
// MEMBER-NEXT:         {
// MEMBER-NEXT:           key.nameoffset: 4,
// MEMBER-NEXT:           key.namelength: 14
// MEMBER-NEXT:         },
// MEMBER-NEXT:         {
// MEMBER-NEXT:           key.nameoffset: 20,
// MEMBER-NEXT:           key.namelength: 13
// MEMBER-NEXT:         },
// MEMBER-NEXT:         {
// MEMBER-NEXT:           key.nameoffset: 35,
// MEMBER-NEXT:           key.namelength: 10
// MEMBER-NEXT:         }
// MEMBER-NEXT:       ],
// MEMBER-NEXT:       key.active_parameter: 0
// MEMBER-NEXT:     },
// MEMBER-NEXT:     {
// MEMBER-NEXT:       key.name: "add(arg1: Double, arg2: Float, arg3: Int) -> Double",
// MEMBER-NEXT:       key.parameters: [
// MEMBER-NEXT:         {
// MEMBER-NEXT:           key.nameoffset: 4,
// MEMBER-NEXT:           key.namelength: 12
// MEMBER-NEXT:         },
// MEMBER-NEXT:         {
// MEMBER-NEXT:           key.nameoffset: 18,
// MEMBER-NEXT:           key.namelength: 11
// MEMBER-NEXT:         },
// MEMBER-NEXT:         {
// MEMBER-NEXT:           key.nameoffset: 31,
// MEMBER-NEXT:           key.namelength: 9
// MEMBER-NEXT:         }
// MEMBER-NEXT:       ],
// MEMBER-NEXT:       key.active_parameter: 0
// MEMBER-NEXT:     },
// MEMBER-NEXT:     {
// MEMBER-NEXT:       key.name: "add(numbers: Double...) -> Double",
// MEMBER-NEXT:       key.parameters: [
// MEMBER-NEXT:         {
// MEMBER-NEXT:           key.nameoffset: 4,
// MEMBER-NEXT:           key.namelength: 18
// MEMBER-NEXT:         }
// MEMBER-NEXT:       ],
// MEMBER-NEXT:       key.active_parameter: 0
// MEMBER-NEXT:     },
// MEMBER-NEXT:     {
// MEMBER-NEXT:       key.name: "add(x: Int, y: Int, with: (Int, Int) -> Int) -> Int",
// MEMBER-NEXT:       key.parameters: [
// MEMBER-NEXT:         {
// MEMBER-NEXT:           key.nameoffset: 4,
// MEMBER-NEXT:           key.namelength: 6
// MEMBER-NEXT:         },
// MEMBER-NEXT:         {
// MEMBER-NEXT:           key.nameoffset: 12,
// MEMBER-NEXT:           key.namelength: 6
// MEMBER-NEXT:         },
// MEMBER-NEXT:         {
// MEMBER-NEXT:           key.nameoffset: 20,
// MEMBER-NEXT:           key.namelength: 23
// MEMBER-NEXT:         }
// MEMBER-NEXT:       ],
// MEMBER-NEXT:       key.active_parameter: 0
// MEMBER-NEXT:     },
// MEMBER-NEXT:     {
// MEMBER-NEXT:       key.name: "add(x: Int) -> (Int) -> Int",
// MEMBER-NEXT:       key.parameters: [
// MEMBER-NEXT:         {
// MEMBER-NEXT:           key.nameoffset: 4,
// MEMBER-NEXT:           key.namelength: 6
// MEMBER-NEXT:         }
// MEMBER-NEXT:       ],
// MEMBER-NEXT:       key.active_parameter: 0
// MEMBER-NEXT:     }
// MEMBER-NEXT:   ],
// MEMBER-NEXT:   key.active_signature: 0
// MEMBER-NEXT: }
