// XFAIL: broken_std_regex
// RUN: %complete-test -top=0 -tok=TOP_LEVEL_0 %s | %FileCheck %s -check-prefix=TOP_LEVEL_0
// RUN: %complete-test -top=0 -tok=TOP_LEVEL_1 %s | %FileCheck %s -check-prefix=TOP_LEVEL_1
// RUN: %complete-test -top=0 -tok=TOP_LEVEL_2 %s | %FileCheck %s -check-prefix=TOP_LEVEL_2
// RUN: %complete-test -top=0 -tok=TOP_LEVEL_3 %s | %FileCheck %s -check-prefix=TOP_LEVEL_3
// RUN: %complete-test -top=0 -group=none -tok=CROSS_CONTEXT_0 %s | %FileCheck %s -check-prefix=CROSS_CONTEXT_0
// RUN: %complete-test -top=0 -group=none -tok=FROM_METHOD_0 %s | %FileCheck %s -check-prefix=FROM_METHOD_0

let valueA = [0]
let valueS = ""
let valueZ = 1

func takeInt(x: Int, y: Int)
func takeIntOpt(x: Int, y: Int?)
func takeString(x: Int, y: String)
func takeAny(x: Int, y: Any)

takeInt(1, y: #^TOP_LEVEL_0^#)
// TOP_LEVEL_0-NOT: nil
// TOP_LEVEL_0: valueZ
// TOP_LEVEL_0: Int
// TOP_LEVEL_0: valueA
// TOP_LEVEL_0: valueS

takeString(1, y: #^TOP_LEVEL_1^#)
// TOP_LEVEL_1: valueS
// TOP_LEVEL_1: String
// TOP_LEVEL_1: valueA
// TOP_LEVEL_1: valueZ

takeAny(1, y: #^TOP_LEVEL_2^#)
// TOP_LEVEL_2: valueA
// TOP_LEVEL_2: valueS
// TOP_LEVEL_2: valueZ

takeIntOpt(1, y: #^TOP_LEVEL_3^#)
// TOP_LEVEL_3: nil
// TOP_LEVEL_3: valueZ
// TOP_LEVEL_3: valueA
// TOP_LEVEL_3: valueS

func testCrossContext(x: Int, y: String, z: Any) {
  takeInt(1, y: #^CROSS_CONTEXT_0^#)
}
// CROSS_CONTEXT_0: x
// CROSS_CONTEXT_0: valueZ
// CROSS_CONTEXT_0: Int
// CROSS_CONTEXT_0: y
// CROSS_CONTEXT_0: z
// CROSS_CONTEXT_0: valueA
// CROSS_CONTEXT_0: valueS

struct FromMethod {
  func valueA() -> [Int] { return [0] }
  func valueS() -> String { return "" }
  func valueZ() -> Int { return 1 }
}

func testFromMethod(x: FromMethod) {
  takeInt(1, y: x.#^FROM_METHOD_0^#)
}
// FROM_METHOD_0: valueZ()
// FROM_METHOD_0: valueA()
// FROM_METHOD_0: valueS()
