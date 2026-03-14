// RUN: %target-swift-ide-test -signature-help -code-completion-token=CURRY_TOPLEVEL -source-filename=%s | %FileCheck %s --check-prefix=CURRY_TOPLEVEL
// RUN: %target-swift-ide-test -signature-help -code-completion-token=CURRY_MEMBER_PARTIAL -source-filename=%s | %FileCheck %s --check-prefix=CURRY_MEMBER_PARTIAL
// RUN: %target-swift-ide-test -signature-help -code-completion-token=CURRY_MEMBER_FULL -source-filename=%s | %FileCheck %s --check-prefix=CURRY_MEMBER_FULL

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
    return try adder(x, y)
  }

  func add(x: Int) -> (Int) -> Int {
    return { (y: Int) in x + y }
  }
}

func topLevelCurried(x: Int) -> (Double) -> (String) -> Void {
  fatalError()
}

func testCurryTopLevel() {
  topLevelCurried(x: 1)(#^CURRY_TOPLEVEL^#)
  // CURRY_TOPLEVEL:     Begin signatures, 1 items
  // CURRY_TOPLEVEL-DAG: Signature[Active]: (<param active>Double</param>) -> (String) -> Void
}

func testCurryMemberPartial() {
  Adder.add(#^CURRY_MEMBER_PARTIAL^#)
  // CURRY_MEMBER_PARTIAL:     Begin signatures, 8 items
  // CURRY_MEMBER_PARTIAL-DAG: Signature[Active]: add(<param name="self" active>_ self: Adder</param>) -> (Int, Int) -> Int
  // CURRY_MEMBER_PARTIAL-DAG: Signature: add(<param name="self" active>_ self: Adder</param>) -> (inout Int) -> ()
  // CURRY_MEMBER_PARTIAL-DAG: Signature: add(<param name="self" active>_ self: Adder</param>) -> (AdditiveArithmetic, AdditiveArithmetic) -> AdditiveArithmetic
  // CURRY_MEMBER_PARTIAL-DAG: Signature: add(<param name="self" active>_ self: Adder</param>) -> (Double?, Float, Int) -> Double
  // CURRY_MEMBER_PARTIAL-DAG: Signature: add(<param name="self" active>_ self: Adder</param>) -> (Double, Float, Int) -> Double
  // CURRY_MEMBER_PARTIAL-DAG: Signature: add(<param name="self" active>_ self: Adder</param>) -> (Double...) -> Double
  // CURRY_MEMBER_PARTIAL-DAG: Signature: add(<param name="self" active>_ self: Adder</param>) -> (Int, Int, (Int, Int) throws -> Int) throws -> Int?
  // CURRY_MEMBER_PARTIAL-DAG: Signature: add(<param name="self" active>_ self: Adder</param>) -> (Int) -> (Int) -> Int
}

func testCurryMemberFull() {
  let adder = Adder()
  Adder.add(adder)(#^CURRY_MEMBER_FULL^#)
  // CURRY_MEMBER_FULL:     Begin signatures, 8 items
  // CURRY_MEMBER_FULL-DAG: Signature[Active]: (<param name="x" active>_ x: Int</param>, <param name="y">to: Int</param>) -> Int
  // CURRY_MEMBER_FULL-DAG: Signature: (<param name="x" active>oneTo: inout Int</param>)
  // CURRY_MEMBER_FULL-DAG: Signature: (<param name="x" active>_ x: AdditiveArithmetic</param>, <param name="y">to: AdditiveArithmetic</param>) -> AdditiveArithmetic
  // CURRY_MEMBER_FULL-DAG: Signature: (<param name="first" active>first: Double!</param>, <param name="second">second: Float</param>, <param name="third">third: Int</param>) -> Double
  // CURRY_MEMBER_FULL-DAG: Signature: (<param name="param1" active>arg1: Double</param>, <param name="arg2">arg2: Float</param>, <param name="param3">arg3: Int</param>) -> Double
  // CURRY_MEMBER_FULL-DAG: Signature: (<param name="numbers" active>numbers: Double...</param>) -> Double
  // CURRY_MEMBER_FULL-DAG: Signature: (<param name="x" active>x: Int</param>, <param name="y">y: Int</param>, <param name="adder">with: (Int, Int) throws -> Int</param>) throws -> Int!
  // CURRY_MEMBER_FULL-DAG: Signature: (<param name="x" active>x: Int</param>) -> (Int) -> Int
}
