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

func testMember() {
  let adder = Adder()
  adder.add()
}

func testCurryPartial() {
  Adder.add()
}

func testCurryFull() {
  Adder.add(adder)()
}

// RUN: %sourcekitd-test -req=signaturehelp -pos=37:13 %s -- %s | %FileCheck -check-prefix=MEMBER %s
// RUN: %sourcekitd-test -req=signaturehelp -pos=41:13 %s -- %s | %FileCheck -check-prefix=CURRY_PARTIAL %s
// RUN: %sourcekitd-test -req=signaturehelp -pos=45:20 %s -- %s | %FileCheck -check-prefix=CURRY_FULL %s

// MEMBER:      {
// MEMBER-NEXT:   key.active_signature: 0,
// MEMBER-NEXT:   key.members: [
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
// MEMBER-NEXT:   ]
// MEMBER-NEXT: }

// CURRY_PARTIAL:      {
// CURRY_PARTIAL-NEXT:   key.active_signature: 0,
// CURRY_PARTIAL-NEXT:   key.members: [
// CURRY_PARTIAL-NEXT:     {
// CURRY_PARTIAL-NEXT:       key.name: "add(_ self: Adder) -> (Int, Int) -> Int",
// CURRY_PARTIAL-NEXT:       key.parameters: [
// CURRY_PARTIAL-NEXT:         {
// CURRY_PARTIAL-NEXT:           key.nameoffset: 4,
// CURRY_PARTIAL-NEXT:           key.namelength: 13
// CURRY_PARTIAL-NEXT:         }
// CURRY_PARTIAL-NEXT:       ],
// CURRY_PARTIAL-NEXT:       key.active_parameter: 0
// CURRY_PARTIAL-NEXT:     },
// CURRY_PARTIAL-NEXT:     {
// CURRY_PARTIAL-NEXT:       key.name: "add(_ self: Adder) -> (inout Int) -> ()",
// CURRY_PARTIAL-NEXT:       key.parameters: [
// CURRY_PARTIAL-NEXT:         {
// CURRY_PARTIAL-NEXT:           key.nameoffset: 4,
// CURRY_PARTIAL-NEXT:           key.namelength: 13
// CURRY_PARTIAL-NEXT:         }
// CURRY_PARTIAL-NEXT:       ],
// CURRY_PARTIAL-NEXT:       key.active_parameter: 0
// CURRY_PARTIAL-NEXT:     },
// CURRY_PARTIAL-NEXT:     {
// CURRY_PARTIAL-NEXT:       key.name: "add(_ self: Adder) -> (AdditiveArithmetic, AdditiveArithmetic) -> AdditiveArithmetic",
// CURRY_PARTIAL-NEXT:       key.parameters: [
// CURRY_PARTIAL-NEXT:         {
// CURRY_PARTIAL-NEXT:           key.nameoffset: 4,
// CURRY_PARTIAL-NEXT:           key.namelength: 13
// CURRY_PARTIAL-NEXT:         }
// CURRY_PARTIAL-NEXT:       ],
// CURRY_PARTIAL-NEXT:       key.active_parameter: 0
// CURRY_PARTIAL-NEXT:     },
// CURRY_PARTIAL-NEXT:     {
// CURRY_PARTIAL-NEXT:       key.name: "add(_ self: Adder) -> (Double?, Float, Int) -> Double",
// CURRY_PARTIAL-NEXT:       key.parameters: [
// CURRY_PARTIAL-NEXT:         {
// CURRY_PARTIAL-NEXT:           key.nameoffset: 4,
// CURRY_PARTIAL-NEXT:           key.namelength: 13
// CURRY_PARTIAL-NEXT:         }
// CURRY_PARTIAL-NEXT:       ],
// CURRY_PARTIAL-NEXT:       key.active_parameter: 0
// CURRY_PARTIAL-NEXT:     },
// CURRY_PARTIAL-NEXT:     {
// CURRY_PARTIAL-NEXT:       key.name: "add(_ self: Adder) -> (Double, Float, Int) -> Double",
// CURRY_PARTIAL-NEXT:       key.parameters: [
// CURRY_PARTIAL-NEXT:         {
// CURRY_PARTIAL-NEXT:           key.nameoffset: 4,
// CURRY_PARTIAL-NEXT:           key.namelength: 13
// CURRY_PARTIAL-NEXT:         }
// CURRY_PARTIAL-NEXT:       ],
// CURRY_PARTIAL-NEXT:       key.active_parameter: 0
// CURRY_PARTIAL-NEXT:     },
// CURRY_PARTIAL-NEXT:     {
// CURRY_PARTIAL-NEXT:       key.name: "add(_ self: Adder) -> (Double...) -> Double",
// CURRY_PARTIAL-NEXT:       key.parameters: [
// CURRY_PARTIAL-NEXT:         {
// CURRY_PARTIAL-NEXT:           key.nameoffset: 4,
// CURRY_PARTIAL-NEXT:           key.namelength: 13
// CURRY_PARTIAL-NEXT:         }
// CURRY_PARTIAL-NEXT:       ],
// CURRY_PARTIAL-NEXT:       key.active_parameter: 0
// CURRY_PARTIAL-NEXT:     },
// CURRY_PARTIAL-NEXT:     {
// CURRY_PARTIAL-NEXT:       key.name: "add(_ self: Adder) -> (Int, Int, (Int, Int) -> Int) -> Int",
// CURRY_PARTIAL-NEXT:       key.parameters: [
// CURRY_PARTIAL-NEXT:         {
// CURRY_PARTIAL-NEXT:           key.nameoffset: 4,
// CURRY_PARTIAL-NEXT:           key.namelength: 13
// CURRY_PARTIAL-NEXT:         }
// CURRY_PARTIAL-NEXT:       ],
// CURRY_PARTIAL-NEXT:       key.active_parameter: 0
// CURRY_PARTIAL-NEXT:     },
// CURRY_PARTIAL-NEXT:     {
// CURRY_PARTIAL-NEXT:       key.name: "add(_ self: Adder) -> (Int) -> (Int) -> Int",
// CURRY_PARTIAL-NEXT:       key.parameters: [
// CURRY_PARTIAL-NEXT:         {
// CURRY_PARTIAL-NEXT:           key.nameoffset: 4,
// CURRY_PARTIAL-NEXT:           key.namelength: 13
// CURRY_PARTIAL-NEXT:         }
// CURRY_PARTIAL-NEXT:       ],
// CURRY_PARTIAL-NEXT:       key.active_parameter: 0
// CURRY_PARTIAL-NEXT:     }
// CURRY_PARTIAL-NEXT:   ]
// CURRY_PARTIAL-NEXT: }

// CURRY_FULL:      {
// CURRY_FULL-NEXT:   key.active_signature: 0,
// CURRY_FULL-NEXT:   key.members: [
// CURRY_FULL-NEXT:     {
// CURRY_FULL-NEXT:       key.name: "(Int, to: Int) -> Int",
// CURRY_FULL-NEXT:       key.parameters: [
// CURRY_FULL-NEXT:         {
// CURRY_FULL-NEXT:           key.nameoffset: 1,
// CURRY_FULL-NEXT:           key.namelength: 3
// CURRY_FULL-NEXT:         },
// CURRY_FULL-NEXT:         {
// CURRY_FULL-NEXT:           key.nameoffset: 6,
// CURRY_FULL-NEXT:           key.namelength: 7
// CURRY_FULL-NEXT:         }
// CURRY_FULL-NEXT:       ],
// CURRY_FULL-NEXT:       key.active_parameter: 0
// CURRY_FULL-NEXT:     },
// CURRY_FULL-NEXT:     {
// CURRY_FULL-NEXT:       key.name: "(oneTo: inout Int)",
// CURRY_FULL-NEXT:       key.parameters: [
// CURRY_FULL-NEXT:         {
// CURRY_FULL-NEXT:           key.nameoffset: 1,
// CURRY_FULL-NEXT:           key.namelength: 16
// CURRY_FULL-NEXT:         }
// CURRY_FULL-NEXT:       ],
// CURRY_FULL-NEXT:       key.active_parameter: 0
// CURRY_FULL-NEXT:     },
// CURRY_FULL-NEXT:     {
// CURRY_FULL-NEXT:       key.name: "(_, to: _) -> _",
// CURRY_FULL-NEXT:       key.parameters: [
// CURRY_FULL-NEXT:         {
// CURRY_FULL-NEXT:           key.nameoffset: 1,
// CURRY_FULL-NEXT:           key.namelength: 1
// CURRY_FULL-NEXT:         },
// CURRY_FULL-NEXT:         {
// CURRY_FULL-NEXT:           key.nameoffset: 4,
// CURRY_FULL-NEXT:           key.namelength: 5
// CURRY_FULL-NEXT:         }
// CURRY_FULL-NEXT:       ],
// CURRY_FULL-NEXT:       key.active_parameter: 0
// CURRY_FULL-NEXT:     },
// CURRY_FULL-NEXT:     {
// CURRY_FULL-NEXT:       key.name: "(first: Double?, second: Float, third: Int) -> Double",
// CURRY_FULL-NEXT:       key.parameters: [
// CURRY_FULL-NEXT:         {
// CURRY_FULL-NEXT:           key.nameoffset: 1,
// CURRY_FULL-NEXT:           key.namelength: 14
// CURRY_FULL-NEXT:         },
// CURRY_FULL-NEXT:         {
// CURRY_FULL-NEXT:           key.nameoffset: 17,
// CURRY_FULL-NEXT:           key.namelength: 13
// CURRY_FULL-NEXT:         },
// CURRY_FULL-NEXT:         {
// CURRY_FULL-NEXT:           key.nameoffset: 32,
// CURRY_FULL-NEXT:           key.namelength: 10
// CURRY_FULL-NEXT:         }
// CURRY_FULL-NEXT:       ],
// CURRY_FULL-NEXT:       key.active_parameter: 0
// CURRY_FULL-NEXT:     },
// CURRY_FULL-NEXT:     {
// CURRY_FULL-NEXT:       key.name: "(arg1: Double, arg2: Float, arg3: Int) -> Double",
// CURRY_FULL-NEXT:       key.parameters: [
// CURRY_FULL-NEXT:         {
// CURRY_FULL-NEXT:           key.nameoffset: 1,
// CURRY_FULL-NEXT:           key.namelength: 12
// CURRY_FULL-NEXT:         },
// CURRY_FULL-NEXT:         {
// CURRY_FULL-NEXT:           key.nameoffset: 15,
// CURRY_FULL-NEXT:           key.namelength: 11
// CURRY_FULL-NEXT:         },
// CURRY_FULL-NEXT:         {
// CURRY_FULL-NEXT:           key.nameoffset: 28,
// CURRY_FULL-NEXT:           key.namelength: 9
// CURRY_FULL-NEXT:         }
// CURRY_FULL-NEXT:       ],
// CURRY_FULL-NEXT:       key.active_parameter: 0
// CURRY_FULL-NEXT:     },
// CURRY_FULL-NEXT:     {
// CURRY_FULL-NEXT:       key.name: "(numbers: Double...) -> Double",
// CURRY_FULL-NEXT:       key.parameters: [
// CURRY_FULL-NEXT:         {
// CURRY_FULL-NEXT:           key.nameoffset: 1,
// CURRY_FULL-NEXT:           key.namelength: 18
// CURRY_FULL-NEXT:         }
// CURRY_FULL-NEXT:       ],
// CURRY_FULL-NEXT:       key.active_parameter: 0
// CURRY_FULL-NEXT:     },
// CURRY_FULL-NEXT:     {
// CURRY_FULL-NEXT:       key.name: "(x: Int, y: Int, with: (Int, Int) -> Int) -> Int",
// CURRY_FULL-NEXT:       key.parameters: [
// CURRY_FULL-NEXT:         {
// CURRY_FULL-NEXT:           key.nameoffset: 1,
// CURRY_FULL-NEXT:           key.namelength: 6
// CURRY_FULL-NEXT:         },
// CURRY_FULL-NEXT:         {
// CURRY_FULL-NEXT:           key.nameoffset: 9,
// CURRY_FULL-NEXT:           key.namelength: 6
// CURRY_FULL-NEXT:         },
// CURRY_FULL-NEXT:         {
// CURRY_FULL-NEXT:           key.nameoffset: 17,
// CURRY_FULL-NEXT:           key.namelength: 23
// CURRY_FULL-NEXT:         }
// CURRY_FULL-NEXT:       ],
// CURRY_FULL-NEXT:       key.active_parameter: 0
// CURRY_FULL-NEXT:     },
// CURRY_FULL-NEXT:     {
// CURRY_FULL-NEXT:       key.name: "(x: Int) -> (Int) -> Int",
// CURRY_FULL-NEXT:       key.parameters: [
// CURRY_FULL-NEXT:         {
// CURRY_FULL-NEXT:           key.nameoffset: 1,
// CURRY_FULL-NEXT:           key.namelength: 6
// CURRY_FULL-NEXT:         }
// CURRY_FULL-NEXT:       ],
// CURRY_FULL-NEXT:       key.active_parameter: 0
// CURRY_FULL-NEXT:     }
// CURRY_FULL-NEXT:   ]
// CURRY_FULL-NEXT: }
