// RUN: %target-swift-ide-test -signature-help -code-completion-token=MEMBER_FUNC -source-filename=%s | %FileCheck %s --check-prefix=MEMBER_FUNC

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
adder.add(#^MEMBER_FUNC^#)
// MEMBER_FUNC:     Begin signatures, 8 items
// MEMBER_FUNC-DAG: Signature[Active]: add(<param name="x" active>_ x: Int</param>, <param name="y">to: Int</param>) -> Int
// MEMBER_FUNC-DAG: Signature: add(<param name="x" active>oneTo: inout Int</param>)
// MEMBER_FUNC-DAG: Signature: add(<param name="x" active>_ x: AdditiveArithmetic</param>, <param name="y">to: AdditiveArithmetic</param>) -> AdditiveArithmetic
// MEMBER_FUNC-DAG: Signature: add(<param name="first" active>first: Double!</param>, <param name="second">second: Float</param>, <param name="third">third: Int</param>) -> Double
// MEMBER_FUNC-DAG: Signature: add(<param name="param1" active>arg1: Double</param>, <param name="arg2">arg2: Float</param>, <param name="param3">arg3: Int</param>) -> Double
// MEMBER_FUNC-DAG: Signature: add(<param name="numbers" active>numbers: Double...</param>) -> Double
// MEMBER_FUNC-DAG: Signature: add(<param name="x" active>x: Int</param>, <param name="y">y: Int</param>, <param name="adder">with: (Int, Int) -> Int</param>) -> Int
// MEMBER_FUNC-DAG: Signature: add(<param name="x" active>x: Int</param>) -> (Int) -> Int
