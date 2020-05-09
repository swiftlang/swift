// XFAIL: broken_std_regex
// RUN: %complete-test -tok=INT_OPERATORS %s | %FileCheck %s
// RUN: %complete-test -add-inner-results -tok=INT_OPERATORS_INNER %s | %FileCheck %s -check-prefix=INNER
// RUN: %complete-test -raw -hide-none -tok=INT_OPERATORS %s | %FileCheck %s -check-prefix=RAW
// RUN: %complete-test -tok=BOOL_OPERATORS %s | %FileCheck %s -check-prefix=BOOL
// RUN: %complete-test -tok=OPT_OPERATORS %s | %FileCheck %s -check-prefix=OPT
// RUN: %complete-test -tok=KNOWN_OPERATORS_1 %s | %FileCheck %s -check-prefix=KNOWN
// RUN: %complete-test -tok=KNOWN_OPERATORS_2 %s | %FileCheck %s -check-prefix=KNOWN

struct MyInt {
  var bigPowers: Int { return 1 }
}
func +(x: MyInt, y: MyInt) -> MyInt { return x }
postfix operator ++
postfix func ++(x: inout MyInt) -> MyInt { return x }
func !=(x: MyInt, y: MyInt) -> Bool { return true }

let xxxx = 1
func test1(x: inout MyInt) {
  x#^INT_OPERATORS^#
}
// CHECK: .
// CHECK: =
// CHECK: !=
// CHECK: +
// CHECK: ++

func test2(x: inout MyInt) {
  #^INT_OPERATORS_INNER,x^#
}
// INNER: x.
// INNER: x=
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

func test4(x: inout MyBool?) {
  x#^OPT_OPERATORS^#
}
// OPT: .
// OPT: ?.
// OPT: !
// OPT: =
// OPT: ==
// OPT: !=
// OPT: ??

class DoesEverything {
  var member: Int = 0
}
func !=(x: DoesEverything, y: DoesEverything) {}
func %(x: DoesEverything, y: DoesEverything) {}
func %=(x: DoesEverything, y: DoesEverything) {}
func &(x: DoesEverything, y: DoesEverything) {}
func &&(x: DoesEverything, y: DoesEverything) {}
func &*(x: DoesEverything, y: DoesEverything) {}
func &+(x: DoesEverything, y: DoesEverything) {}
func &-(x: DoesEverything, y: DoesEverything) {}
func &=(x: DoesEverything, y: DoesEverything) {}
func *(x: DoesEverything, y: DoesEverything) {}
func *=(x: DoesEverything, y: DoesEverything) {}
func +(x: DoesEverything, y: DoesEverything) {}
func +=(x: DoesEverything, y: DoesEverything) {}
func -(x: DoesEverything, y: DoesEverything) {}
func -=(x: DoesEverything, y: DoesEverything) {}
func ...(x: DoesEverything, y: DoesEverything) {}
func ..<(x: DoesEverything, y: DoesEverything) {}
func /(x: DoesEverything, y: DoesEverything) {}
func /=(x: DoesEverything, y: DoesEverything) {}
func <(x: DoesEverything, y: DoesEverything) {}
func <<(x: DoesEverything, y: DoesEverything) {}
func <<=(x: DoesEverything, y: DoesEverything) {}
func <=(x: DoesEverything, y: DoesEverything) {}
func ==(x: DoesEverything, y: DoesEverything) {}
func >(x: DoesEverything, y: DoesEverything) {}
func >=(x: DoesEverything, y: DoesEverything) {}
func >>(x: DoesEverything, y: DoesEverything) {}
func >>=(x: DoesEverything, y: DoesEverything) {}
func ^(x: DoesEverything, y: DoesEverything) {}
func ^=(x: DoesEverything, y: DoesEverything) {}
func |(x: DoesEverything, y: DoesEverything) {}
func |=(x: DoesEverything, y: DoesEverything) {}
func ||(x: DoesEverything, y: DoesEverything) {}
func ~=(x: DoesEverything, y: DoesEverything) {}

// Custom
infix operator *** { associativity left precedence 140 }
func ***(x: DoesEverything, y: DoesEverything) {}

func test5(x: DoesEverything) {
  x#^KNOWN_OPERATORS_1^#
}
func test6(x: DoesEverything) {
  #^KNOWN_OPERATORS_2,x^#
}
// KNOWN: .
// KNOWN: ==
// KNOWN: !=
// KNOWN: <
// KNOWN: >
// KNOWN: <=
// KNOWN: >=
// KNOWN: +
// KNOWN: -
// KNOWN: *
// KNOWN: /
// KNOWN: %
// KNOWN: +=
// KNOWN: -=
// KNOWN: *=
// KNOWN: /=
// KNOWN: %=
// KNOWN: &&
// KNOWN: ||

// Custom/unknown operators.
// KNOWN: ***

// KNOWN: &
// KNOWN: |
// KNOWN: ^
// KNOWN: <<
// KNOWN: >>
// KNOWN: &=
// KNOWN: |=
// KNOWN: ^=
// KNOWN: <<=
// KNOWN: >>=
// KNOWN: ...
// KNOWN: ..<
// KNOWN: &*
// KNOWN: &+
// KNOWN: &-
// KNOWN: ===
// KNOWN: !==
// KNOWN: ~=
