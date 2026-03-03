// RUN: %target-swift-ide-test -signature-help -code-completion-token=DEFAULT_ARGS -source-filename=%s | %FileCheck %s --check-prefix=DEFAULT_ARGS

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

add(#^DEFAULT_ARGS^#)
// DEFAULT_ARGS:     Begin signatures, 8 items
// DEFAULT_ARGS-DAG: Signature[Active]: add(<param name="x" active>_ x: Int = 10</param>, <param name="y">to: Int</param>) -> Int
// DEFAULT_ARGS-DAG: Signature: add(<param name="x" active>oneTo: inout Int</param>)
// DEFAULT_ARGS-DAG: Signature: add(<param name="x" active>_ x: Int</param>, <param name="y">to: Int? = nil</param>) -> String
// DEFAULT_ARGS-DAG: Signature: add(<param name="first" active>first: Double!</param>, <param name="second">second: Float = .pi</param>, <param name="third">third: Int</param>) -> Double
// DEFAULT_ARGS-DAG: Signature: add(<param name="s" active>s: S = S(a: false)</param>) -> Double
// DEFAULT_ARGS-DAG: Signature: add(<param name="x" active>x: Int</param>, <param name="y">y: Int</param>, <param name="adder">with: (Int, Int) -> Int = { $0 + $1 }</param>) -> Int
// DEFAULT_ARGS-DAG: Signature: add(<param name="x" active>x: Int = importantValue</param>)
// DEFAULT_ARGS-DAG: Signature: add(<param name="x" active>x: Int</param>, <param name="line">line: UInt = #line</param>, <param name="file">file: StaticString = #file</param>)
