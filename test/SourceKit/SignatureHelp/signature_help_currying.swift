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

  func add(x: Int, y: Int, with adder: (Int, Int) throws -> Int) rethrows -> Int! {
    return adder(x, y)
  }

  func add(x: Int) -> (Int) -> Int {
    return { (y: Int) in x + y }
  }
}

func topLevelCurried(x: Int) -> (Double) -> (String) -> Void {
  fatalError()
}

func testCurryTopLevel() {
  topLevelCurried(x: 1)()
}

func testCurryMemberPartial() {
  Adder.add()
}

func testCurryMemberFull() {
  let adder = Adder()
  Adder.add(adder)()
}

// RUN: %sourcekitd-test -req=signaturehelp -pos=40:25 %s -- %s | %FileCheck -check-prefix=CURRY_TOPLEVEL %s
// RUN: %sourcekitd-test -req=signaturehelp -pos=44:13 %s -- %s | %FileCheck -check-prefix=CURRY_MEMBER_PARTIAL %s
// RUN: %sourcekitd-test -req=signaturehelp -pos=49:20 %s -- %s | %FileCheck -check-prefix=CURRY_MEMBER_FULL %s

// CURRY_TOPLEVEL:      {
// CHECK_TOPLEVEL-NEXT:   key.active_signature: 0,
// CHECK_TOPLEVEL-NEXT:   key.members: [
// CHECK_TOPLEVEL-NEXT:     {
// CHECK_TOPLEVEL-NEXT:       key.name: "(Double) -> (String) -> Void",
// CHECK_TOPLEVEL-NEXT:       key.parameters: [
// CHECK_TOPLEVEL-NEXT:         {
// CHECK_TOPLEVEL-NEXT:           key.nameoffset: 1,
// CHECK_TOPLEVEL-NEXT:           key.namelength: 6
// CHECK_TOPLEVEL-NEXT:         }
// CHECK_TOPLEVEL-NEXT:       ],
// CHECK_TOPLEVEL-NEXT:       key.active_parameter: 0
// CHECK_TOPLEVEL-NEXT:     }
// CHECK_TOPLEVEL-NEXT:   ]
// CHECK_TOPLEVEL-NEXT: }

// CURRY_MEMBER_PARTIAL:      {
// CURRY_MEMBER_PARTIAL-NEXT:   key.active_signature: 0,
// CURRY_MEMBER_PARTIAL-NEXT:   key.members: [
// CURRY_MEMBER_PARTIAL-NEXT:     {
// CURRY_MEMBER_PARTIAL-NEXT:       key.name: "add(_ self: Adder) -> (Int, Int) -> Int",
// CURRY_MEMBER_PARTIAL-NEXT:       key.parameters: [
// CURRY_MEMBER_PARTIAL-NEXT:         {
// CURRY_MEMBER_PARTIAL-NEXT:           key.nameoffset: 4,
// CURRY_MEMBER_PARTIAL-NEXT:           key.namelength: 13
// CURRY_MEMBER_PARTIAL-NEXT:         }
// CURRY_MEMBER_PARTIAL-NEXT:       ],
// CURRY_MEMBER_PARTIAL-NEXT:       key.active_parameter: 0
// CURRY_MEMBER_PARTIAL-NEXT:     },
// CURRY_MEMBER_PARTIAL-NEXT:     {
// CURRY_MEMBER_PARTIAL-NEXT:       key.name: "add(_ self: Adder) -> (inout Int) -> ()",
// CURRY_MEMBER_PARTIAL-NEXT:       key.parameters: [
// CURRY_MEMBER_PARTIAL-NEXT:         {
// CURRY_MEMBER_PARTIAL-NEXT:           key.nameoffset: 4,
// CURRY_MEMBER_PARTIAL-NEXT:           key.namelength: 13
// CURRY_MEMBER_PARTIAL-NEXT:         }
// CURRY_MEMBER_PARTIAL-NEXT:       ],
// CURRY_MEMBER_PARTIAL-NEXT:       key.active_parameter: 0
// CURRY_MEMBER_PARTIAL-NEXT:     },
// CURRY_MEMBER_PARTIAL-NEXT:     {
// CURRY_MEMBER_PARTIAL-NEXT:       key.name: "add(_ self: Adder) -> (AdditiveArithmetic, AdditiveArithmetic) -> AdditiveArithmetic",
// CURRY_MEMBER_PARTIAL-NEXT:       key.parameters: [
// CURRY_MEMBER_PARTIAL-NEXT:         {
// CURRY_MEMBER_PARTIAL-NEXT:           key.nameoffset: 4,
// CURRY_MEMBER_PARTIAL-NEXT:           key.namelength: 13
// CURRY_MEMBER_PARTIAL-NEXT:         }
// CURRY_MEMBER_PARTIAL-NEXT:       ],
// CURRY_MEMBER_PARTIAL-NEXT:       key.active_parameter: 0
// CURRY_MEMBER_PARTIAL-NEXT:     },
// CURRY_MEMBER_PARTIAL-NEXT:     {
// CURRY_MEMBER_PARTIAL-NEXT:       key.name: "add(_ self: Adder) -> (Double?, Float, Int) -> Double",
// CURRY_MEMBER_PARTIAL-NEXT:       key.parameters: [
// CURRY_MEMBER_PARTIAL-NEXT:         {
// CURRY_MEMBER_PARTIAL-NEXT:           key.nameoffset: 4,
// CURRY_MEMBER_PARTIAL-NEXT:           key.namelength: 13
// CURRY_MEMBER_PARTIAL-NEXT:         }
// CURRY_MEMBER_PARTIAL-NEXT:       ],
// CURRY_MEMBER_PARTIAL-NEXT:       key.active_parameter: 0
// CURRY_MEMBER_PARTIAL-NEXT:     },
// CURRY_MEMBER_PARTIAL-NEXT:     {
// CURRY_MEMBER_PARTIAL-NEXT:       key.name: "add(_ self: Adder) -> (Double, Float, Int) -> Double",
// CURRY_MEMBER_PARTIAL-NEXT:       key.parameters: [
// CURRY_MEMBER_PARTIAL-NEXT:         {
// CURRY_MEMBER_PARTIAL-NEXT:           key.nameoffset: 4,
// CURRY_MEMBER_PARTIAL-NEXT:           key.namelength: 13
// CURRY_MEMBER_PARTIAL-NEXT:         }
// CURRY_MEMBER_PARTIAL-NEXT:       ],
// CURRY_MEMBER_PARTIAL-NEXT:       key.active_parameter: 0
// CURRY_MEMBER_PARTIAL-NEXT:     },
// CURRY_MEMBER_PARTIAL-NEXT:     {
// CURRY_MEMBER_PARTIAL-NEXT:       key.name: "add(_ self: Adder) -> (Double...) -> Double",
// CURRY_MEMBER_PARTIAL-NEXT:       key.parameters: [
// CURRY_MEMBER_PARTIAL-NEXT:         {
// CURRY_MEMBER_PARTIAL-NEXT:           key.nameoffset: 4,
// CURRY_MEMBER_PARTIAL-NEXT:           key.namelength: 13
// CURRY_MEMBER_PARTIAL-NEXT:         }
// CURRY_MEMBER_PARTIAL-NEXT:       ],
// CURRY_MEMBER_PARTIAL-NEXT:       key.active_parameter: 0
// CURRY_MEMBER_PARTIAL-NEXT:     },
// CURRY_MEMBER_PARTIAL-NEXT:     {
// CURRY_MEMBER_PARTIAL-NEXT:       key.name: "add(_ self: Adder) -> (Int, Int, (Int, Int) throws -> Int) throws -> Int?",
// CURRY_MEMBER_PARTIAL-NEXT:       key.parameters: [
// CURRY_MEMBER_PARTIAL-NEXT:         {
// CURRY_MEMBER_PARTIAL-NEXT:           key.nameoffset: 4,
// CURRY_MEMBER_PARTIAL-NEXT:           key.namelength: 13
// CURRY_MEMBER_PARTIAL-NEXT:         }
// CURRY_MEMBER_PARTIAL-NEXT:       ],
// CURRY_MEMBER_PARTIAL-NEXT:       key.active_parameter: 0
// CURRY_MEMBER_PARTIAL-NEXT:     },
// CURRY_MEMBER_PARTIAL-NEXT:     {
// CURRY_MEMBER_PARTIAL-NEXT:       key.name: "add(_ self: Adder) -> (Int) -> (Int) -> Int",
// CURRY_MEMBER_PARTIAL-NEXT:       key.parameters: [
// CURRY_MEMBER_PARTIAL-NEXT:         {
// CURRY_MEMBER_PARTIAL-NEXT:           key.nameoffset: 4,
// CURRY_MEMBER_PARTIAL-NEXT:           key.namelength: 13
// CURRY_MEMBER_PARTIAL-NEXT:         }
// CURRY_MEMBER_PARTIAL-NEXT:       ],
// CURRY_MEMBER_PARTIAL-NEXT:       key.active_parameter: 0
// CURRY_MEMBER_PARTIAL-NEXT:     }
// CURRY_MEMBER_PARTIAL-NEXT:   ]
// CURRY_MEMBER_PARTIAL-NEXT: }

// CURRY_MEMBER_FULL:      {
// CURRY_MEMBER_FULL-NEXT:   key.active_signature: 0,
// CURRY_MEMBER_FULL-NEXT:   key.members: [
// CURRY_MEMBER_FULL-NEXT:     {
// CURRY_MEMBER_FULL-NEXT:       key.name: "(_ x: Int, to: Int) -> Int",
// CURRY_MEMBER_FULL-NEXT:       key.parameters: [
// CURRY_MEMBER_FULL-NEXT:         {
// CURRY_MEMBER_FULL-NEXT:           key.nameoffset: 1,
// CURRY_MEMBER_FULL-NEXT:           key.namelength: 8
// CURRY_MEMBER_FULL-NEXT:         },
// CURRY_MEMBER_FULL-NEXT:         {
// CURRY_MEMBER_FULL-NEXT:           key.nameoffset: 11,
// CURRY_MEMBER_FULL-NEXT:           key.namelength: 7
// CURRY_MEMBER_FULL-NEXT:         }
// CURRY_MEMBER_FULL-NEXT:       ],
// CURRY_MEMBER_FULL-NEXT:       key.active_parameter: 0
// CURRY_MEMBER_FULL-NEXT:     },
// CURRY_MEMBER_FULL-NEXT:     {
// CURRY_MEMBER_FULL-NEXT:       key.name: "(oneTo: inout Int)",
// CURRY_MEMBER_FULL-NEXT:       key.parameters: [
// CURRY_MEMBER_FULL-NEXT:         {
// CURRY_MEMBER_FULL-NEXT:           key.nameoffset: 1,
// CURRY_MEMBER_FULL-NEXT:           key.namelength: 16
// CURRY_MEMBER_FULL-NEXT:         }
// CURRY_MEMBER_FULL-NEXT:       ],
// CURRY_MEMBER_FULL-NEXT:       key.active_parameter: 0
// CURRY_MEMBER_FULL-NEXT:     },
// CURRY_MEMBER_FULL-NEXT:     {
// CURRY_MEMBER_FULL-NEXT:       key.name: "(_ x: AdditiveArithmetic, to: AdditiveArithmetic) -> AdditiveArithmetic",
// CURRY_MEMBER_FULL-NEXT:       key.parameters: [
// CURRY_MEMBER_FULL-NEXT:         {
// CURRY_MEMBER_FULL-NEXT:           key.nameoffset: 1,
// CURRY_MEMBER_FULL-NEXT:           key.namelength: 23
// CURRY_MEMBER_FULL-NEXT:         },
// CURRY_MEMBER_FULL-NEXT:         {
// CURRY_MEMBER_FULL-NEXT:           key.nameoffset: 26,
// CURRY_MEMBER_FULL-NEXT:           key.namelength: 22
// CURRY_MEMBER_FULL-NEXT:         }
// CURRY_MEMBER_FULL-NEXT:       ],
// CURRY_MEMBER_FULL-NEXT:       key.active_parameter: 0
// CURRY_MEMBER_FULL-NEXT:     },
// CURRY_MEMBER_FULL-NEXT:     {
// CURRY_MEMBER_FULL-NEXT:       key.name: "(first: Double?, second: Float, third: Int) -> Double",
// CURRY_MEMBER_FULL-NEXT:       key.parameters: [
// CURRY_MEMBER_FULL-NEXT:         {
// CURRY_MEMBER_FULL-NEXT:           key.nameoffset: 1,
// CURRY_MEMBER_FULL-NEXT:           key.namelength: 14
// CURRY_MEMBER_FULL-NEXT:         },
// CURRY_MEMBER_FULL-NEXT:         {
// CURRY_MEMBER_FULL-NEXT:           key.nameoffset: 17,
// CURRY_MEMBER_FULL-NEXT:           key.namelength: 13
// CURRY_MEMBER_FULL-NEXT:         },
// CURRY_MEMBER_FULL-NEXT:         {
// CURRY_MEMBER_FULL-NEXT:           key.nameoffset: 32,
// CURRY_MEMBER_FULL-NEXT:           key.namelength: 10
// CURRY_MEMBER_FULL-NEXT:         }
// CURRY_MEMBER_FULL-NEXT:       ],
// CURRY_MEMBER_FULL-NEXT:       key.active_parameter: 0
// CURRY_MEMBER_FULL-NEXT:     },
// CURRY_MEMBER_FULL-NEXT:     {
// CURRY_MEMBER_FULL-NEXT:       key.name: "(arg1: Double, arg2: Float, arg3: Int) -> Double",
// CURRY_MEMBER_FULL-NEXT:       key.parameters: [
// CURRY_MEMBER_FULL-NEXT:         {
// CURRY_MEMBER_FULL-NEXT:           key.nameoffset: 1,
// CURRY_MEMBER_FULL-NEXT:           key.namelength: 12
// CURRY_MEMBER_FULL-NEXT:         },
// CURRY_MEMBER_FULL-NEXT:         {
// CURRY_MEMBER_FULL-NEXT:           key.nameoffset: 15,
// CURRY_MEMBER_FULL-NEXT:           key.namelength: 11
// CURRY_MEMBER_FULL-NEXT:         },
// CURRY_MEMBER_FULL-NEXT:         {
// CURRY_MEMBER_FULL-NEXT:           key.nameoffset: 28,
// CURRY_MEMBER_FULL-NEXT:           key.namelength: 9
// CURRY_MEMBER_FULL-NEXT:         }
// CURRY_MEMBER_FULL-NEXT:       ],
// CURRY_MEMBER_FULL-NEXT:       key.active_parameter: 0
// CURRY_MEMBER_FULL-NEXT:     },
// CURRY_MEMBER_FULL-NEXT:     {
// CURRY_MEMBER_FULL-NEXT:       key.name: "(numbers: Double...) -> Double",
// CURRY_MEMBER_FULL-NEXT:       key.parameters: [
// CURRY_MEMBER_FULL-NEXT:         {
// CURRY_MEMBER_FULL-NEXT:           key.nameoffset: 1,
// CURRY_MEMBER_FULL-NEXT:           key.namelength: 18
// CURRY_MEMBER_FULL-NEXT:         }
// CURRY_MEMBER_FULL-NEXT:       ],
// CURRY_MEMBER_FULL-NEXT:       key.active_parameter: 0
// CURRY_MEMBER_FULL-NEXT:     },
// CURRY_MEMBER_FULL-NEXT:     {
// CURRY_MEMBER_FULL-NEXT:       key.name: "(x: Int, y: Int, with: (Int, Int) throws -> Int) throws -> Int?",
// CURRY_MEMBER_FULL-NEXT:       key.parameters: [
// CURRY_MEMBER_FULL-NEXT:         {
// CURRY_MEMBER_FULL-NEXT:           key.nameoffset: 1,
// CURRY_MEMBER_FULL-NEXT:           key.namelength: 6
// CURRY_MEMBER_FULL-NEXT:         },
// CURRY_MEMBER_FULL-NEXT:         {
// CURRY_MEMBER_FULL-NEXT:           key.nameoffset: 9,
// CURRY_MEMBER_FULL-NEXT:           key.namelength: 6
// CURRY_MEMBER_FULL-NEXT:         },
// CURRY_MEMBER_FULL-NEXT:         {
// CURRY_MEMBER_FULL-NEXT:           key.nameoffset: 17,
// CURRY_MEMBER_FULL-NEXT:           key.namelength: 30
// CURRY_MEMBER_FULL-NEXT:         }
// CURRY_MEMBER_FULL-NEXT:       ],
// CURRY_MEMBER_FULL-NEXT:       key.active_parameter: 0
// CURRY_MEMBER_FULL-NEXT:     },
// CURRY_MEMBER_FULL-NEXT:     {
// CURRY_MEMBER_FULL-NEXT:       key.name: "(x: Int) -> (Int) -> Int",
// CURRY_MEMBER_FULL-NEXT:       key.parameters: [
// CURRY_MEMBER_FULL-NEXT:         {
// CURRY_MEMBER_FULL-NEXT:           key.nameoffset: 1,
// CURRY_MEMBER_FULL-NEXT:           key.namelength: 6
// CURRY_MEMBER_FULL-NEXT:         }
// CURRY_MEMBER_FULL-NEXT:       ],
// CURRY_MEMBER_FULL-NEXT:       key.active_parameter: 0
// CURRY_MEMBER_FULL-NEXT:     }
// CURRY_MEMBER_FULL-NEXT:   ]
// CURRY_MEMBER_FULL-NEXT: }
