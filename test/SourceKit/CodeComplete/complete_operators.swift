// RUN: %complete-test -tok=INT_OPERATORS %s | FileCheck %s
// RUN: %complete-test -add-inner-results -tok=INT_OPERATORS_INNER %s | FileCheck %s -check-prefix=INNER
// RUN: %complete-test -raw -hide-none -tok=INT_OPERATORS %s | FileCheck %s -check-prefix=RAW
// RUN: %complete-test -tok=BOOL_OPERATORS %s | FileCheck %s -check-prefix=BOOL

struct MyInt {
  var bigPowers: Int { return 1 }
}
func +(x: MyInt, y: MyInt) -> MyInt { return x }
postfix func ++(x: inout MyInt) -> MyInt { return x }
func !=(x: MyInt, y: MyInt) -> Bool { return true }

let xxxx = 1
func test1(var x: MyInt) {
  x#^INT_OPERATORS^#
}
// CHECK: .
// CHECK: !=
// CHECK: +
// CHECK: ++

func test2(var x: MyInt) {
  #^INT_OPERATORS_INNER,x^#
}
// INNER: x.
// INNER: x+
// INNER: x++
// INNER: xxxx
// INNER: x.bigPowers

// RAW: {
// RAW:   key.kind: source.lang.swift.decl.function.operator.infix,
// RAW:   key.name: "!=",
// RAW:   key.sourcetext: " != <#T##MyInt#>",
// RAW:   key.description: "!=",
// RAW:   key.typename: "Bool",
// RAW: {
// RAW:   key.kind: source.lang.swift.decl.function.operator.infix,
// RAW:   key.name: "+",
// RAW:   key.sourcetext: " + <#T##MyInt#>",
// RAW:   key.description: "+",
// RAW:   key.typename: "MyInt",
// RAW: },
// RAW: {
// RAW:   key.kind: source.lang.swift.decl.function.operator.postfix,
// RAW:   key.name: "++",
// RAW:   key.sourcetext: "++",
// RAW:   key.description: "++",
// RAW:   key.typename: "MyInt",
// RAW: },

struct MyBool {
  var foo: Int
}
func &&(x: MyBool, y: MyBool) -> MyBool { return x }
func ||(x: MyBool, y: MyBool) -> MyBool { return x }

func test3(x: MyBool) {
  x#^BOOL_OPERATORS^#
}
// BOOL: .
// BOOL: &&
// BOOL: ||
