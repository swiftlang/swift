// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_1 | %FileCheck %s -check-prefix=POSTFIX_1

// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_2 > %t
// RUN: %FileCheck %s -check-prefix=POSTFIX_2 < %t
// RUN: %FileCheck %s -check-prefix=NEGATIVE_POSTFIX_2 < %t

// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_3 | %FileCheck %s -check-prefix=POSTFIX_3
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_4 | %FileCheck %s -check-prefix=POSTFIX_4
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_5 | %FileCheck %s -check-prefix=POSTFIX_5
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_6 | %FileCheck %s -check-prefix=POSTFIX_6
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_7 | %FileCheck %s -check-prefix=POSTFIX_7
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_8 | %FileCheck %s -check-prefix=POSTFIX_8
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_9 | %FileCheck %s -check-prefix=POSTFIX_9
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=POSTFIX_10 | %FileCheck %s -check-prefix=POSTFIX_10
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=S_POSTFIX_SPACE | %FileCheck %s -check-prefix=S_POSTFIX_SPACE

// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_1 > %t
// RUN: %FileCheck %s -check-prefix=S2_INFIX < %t
// RUN: %FileCheck %s -check-prefix=NEGATIVE_S2_INFIX < %t

// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_2 > %t
// RUN: %FileCheck %s -check-prefix=S2_INFIX_LVALUE < %t
// RUN: %FileCheck %s -check-prefix=NEGATIVE_S2_INFIX_LVALUE < %t

// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_3 | %FileCheck %s -check-prefix=S2_INFIX_LVALUE
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_4 | %FileCheck %s -check-prefix=S2_INFIX
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_5 | %FileCheck %s -check-prefix=S2_INFIX
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_6 | %FileCheck %s -check-prefix=S2_INFIX

// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_7 > %t
// RUN: %FileCheck %s -check-prefix=S2_INFIX_OPTIONAL < %t
// RUN: %FileCheck %s -check-prefix=NEGATIVE_S2_INFIX_OPTIONAL < %t

// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_8 | %FileCheck %s -check-prefix=S3_INFIX_OPTIONAL
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_9 | %FileCheck %s -check-prefix=FOOABLE_INFIX
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_10 | %FileCheck %s -check-prefix=FOOABLE_INFIX
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_11 | %FileCheck %s -check-prefix=INFIX_11
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_12 | %FileCheck %s -check-prefix=INFIX_12
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_13 | %FileCheck %s -check-prefix=NO_OPERATORS
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_14 | %FileCheck %s -check-prefix=NO_OPERATORS

// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_15 | %FileCheck %s -check-prefix=INFIX_15
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_16 | %FileCheck %s -check-prefix=INFIX_16
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_17 | %FileCheck %s -check-prefix=VOID_OPERATORS
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_18 | %FileCheck %s -check-prefix=NO_OPERATORS
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_19 | %FileCheck %s -check-prefix=EMPTYCLASS_INFIX
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_20 | %FileCheck %s -check-prefix=NO_OPERATORS
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_21 | %FileCheck %s -check-prefix=NO_OPERATORS
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_22 | %FileCheck %s -check-prefix=INFIX_22
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=S2_INFIX_SPACE | %FileCheck %s -check-prefix=S2_INFIX_SPACE
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=EXT_INFIX_1 | %FileCheck %s -check-prefix=S2_INFIX
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=EXT_INFIX_2 > %t.ext_infix_2
// RUN: %FileCheck %s -check-prefix=S4_EXT_INFIX < %t.ext_infix_2
// RUN: %FileCheck %s -check-prefix=S4_EXT_INFIX_NEG < %t.ext_infix_2
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=EXT_INFIX_3 | %FileCheck %s -check-prefix=S4_EXT_INFIX_SIMPLE
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=EXT_INFIX_4 | %FileCheck %s -check-prefix=S4_EXT_INFIX_SIMPLE
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=ASSIGN_TUPLE_1| %FileCheck %s -check-prefix=ASSIGN_TUPLE_1
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=ASSIGN_TUPLE_2| %FileCheck %s -check-prefix=ASSIGN_TUPLE_2
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=ASSIGN_TUPLE_3| %FileCheck %s -check-prefix=ASSIGN_TUPLE_1

// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_AUTOCLOSURE_1 | %FileCheck %s -check-prefix=INFIX_AUTOCLOSURE_1
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_AUTOCLOSURE_2 | %FileCheck %s -check-prefix=INFIX_AUTOCLOSURE_1
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_AUTOCLOSURE_3 | %FileCheck %s -check-prefix=INFIX_AUTOCLOSURE_1
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INFIX_AUTOCLOSURE_4 | %FileCheck %s -check-prefix=INFIX_AUTOCLOSURE_1

struct S {}
postfix operator ++ {}
postfix func ++(x: inout S) -> S { return x }

func testPostfix1(x: S) {
  x#^POSTFIX_1^#
}
// POSTFIX_1-NOT: ++

func testPostfix2(x: inout S) {
  x#^POSTFIX_2^#
}
// POSTFIX_2: Begin completions
// POSTFIX_2-DAG: Decl[PostfixOperatorFunction]/CurrModule:  ++[#S#]
// POSTFIX_2: End completions
// NEGATIVE_POSTFIX_2-NOT: --


postfix operator +- {}
postfix func +-(x: S) -> S? { return x }
func testPostfix3(x: S) {
  x#^POSTFIX_3^#
}
// POSTFIX_3: Decl[PostfixOperatorFunction]/CurrModule:  +-[#S?#]

func testPostfix4(x: S?) {
  x#^POSTFIX_4^#
}
// POSTFIX_4: BuiltinOperator/None:  ![#S#]

struct T {}
postfix func +-<G>(x: [G]) -> G { return x! }
func testPostfix5(x: [T]) {
  x#^POSTFIX_5^#
}
// POSTFIX_5: Decl[PostfixOperatorFunction]/CurrModule:  +-[#T#]

protocol Fooable {}
extension Int : Fooable {}
extension Double : Fooable {}

postfix operator *** {}
postfix func ***<G: Fooable>(x: G) -> G { return x }
func testPostfix6() {
  1 + 2 * 3#^POSTFIX_6^#
}
// POSTFIX_6: Decl[PostfixOperatorFunction]/CurrModule:  ***[#Int#]

func testPostfix7() {
  1 + 2 * 3.0#^POSTFIX_7^#
}
// POSTFIX_7: Decl[PostfixOperatorFunction]/CurrModule:  ***[#Double#]

func testPostfix8(x: S) {
  x#^POSTFIX_8^#
}
// POSTFIX_8: Begin completions
// POSTFIX_8: Keyword[self]/CurrNominal: .self[#S#]; name=self

protocol P {
  associatedtype T
  func foo() -> T
}

func testPostfix9<G: P where G.T == Int>(x: G) {
  x.foo()#^POSTFIX_9^#
}
// POSTFIX_9: Decl[PostfixOperatorFunction]/CurrModule: ***[#Int#]

func testPostfix10<G: P where G.T : Fooable>(x: G) {
  x.foo()#^POSTFIX_10^#
}
// POSTFIX_10: Decl[PostfixOperatorFunction]/CurrModule: ***[#G.T#]

func testPostfixSpace(x: inout S) {
  x #^S_POSTFIX_SPACE^#
}
// S_POSTFIX_SPACE: Decl[PostfixOperatorFunction]/CurrModule/Erase[1]:  ++[#S#]


// ===--- Infix operators

struct S2 {}
infix operator ** {
  associativity left
  precedence 123
}
infix operator **= {
  associativity none
  precedence 123
}
func +(x: S2, y: S2) -> S2 { return x }
func **(x: S2, y: Int) -> S2 { return x }
func **=(x: inout S2, y: Int) -> Void { return x }

func testInfix1(x: S2) {
  x#^INFIX_1^#
}
// S2_INFIX: Begin completions
// FIXME: rdar://problem/22997089 - should be CurrModule
// S2_INFIX-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:   + {#S2#}[#S2#]
// S2_INFIX-DAG: Decl[InfixOperatorFunction]/CurrModule:   ** {#Int#}[#S2#]; name=**
// S2_INFIX: End completions
// NEGATIVE_S2_INFIX-NOT: **=
// NEGATIVE_S2_INFIX-NOT: +=
// NEGATIVE_S2_INFIX-NOT: \* {#Int#}
// NEGATIVE_S2_INFIX-NOT: ??
// NEGATIVE_S2_INFIX-NOT: ~=
// NEGATIVE_S2_INFIX-NOT: ~>
// NEGATIVE_S2_INFIX-NOT: = {#

func testInfix2(x: inout S2) {
  x#^INFIX_2^#
}
// S2_INFIX_LVALUE: Begin completions
// FIXME: rdar://problem/22997089 - should be CurrModule
// S2_INFIX_LVALUE-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:   + {#S2#}[#S2#]
// S2_INFIX_LVALUE-DAG: Decl[InfixOperatorFunction]/CurrModule:   ** {#Int#}[#S2#]
// S2_INFIX_LVALUE-DAG: Decl[InfixOperatorFunction]/CurrModule:   **= {#Int#}[#Void#]
// S2_INFIX_LVALUE-DAG: BuiltinOperator/None:                             = {#S2#}[#Void#]
// S2_INFIX_LVALUE: End completions
// NEGATIVE_S2_INFIX_LVALUE-NOT: +=
// NEGATIVE_S2_INFIX_LVALUE-NOT: \* {#Int#}
// NEGATIVE_S2_INFIX_LVALUE-NOT: ??
// NEGATIVE_S2_INFIX_LVALUE-NOT: ~=
// NEGATIVE_S2_INFIX_LVALUE-NOT: ~>

func testInfix3(x: inout S2) {
  x#^INFIX_3^#
}

func testInfix4() {
  S2()#^INFIX_4^#
}

func testInfix5() {
  (S2() + S2())#^INFIX_5^#
}

func testInfix6<T: P where T.T == S2>(x: T) {
  x.foo()#^INFIX_6^#
}

func testInfix7(x: S2?) {
  x#^INFIX_7^#
}
// S2_INFIX_OPTIONAL: Begin completions
// S2_INFIX_OPTIONAL-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  != {#{{.*}}#}[#Bool#]
// S2_INFIX_OPTIONAL-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  == {#{{.*}}#}[#Bool#]
// S2_INFIX_OPTIONAL-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  ?? {#S2#}[#S2#]; name=?? S2
// S2_INFIX_OPTIONAL: End completions
// The equality operators don't come from equatable.
// NEGATIVE_S2_INFIX_OPTIONAL-NOT: == {#S2

struct S3: Equatable {}
func ==(x: S3, y: S3) -> Bool { return true }
func !=(x: S3, y: S3) -> Bool { return false}

func testInfix8(x: S3?) {
  x#^INFIX_8^#
}
// The equality operators come from equatable.
// S3_INFIX_OPTIONAL: Begin completions
// S3_INFIX_OPTIONAL-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  != {#S3?#}[#Bool#]
// S3_INFIX_OPTIONAL-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  == {#S3?#}[#Bool#]
// S3_INFIX_OPTIONAL: End completions

infix operator **** {
  associativity left
  precedence 123
}
func ****<T: Fooable>(x: T, y: T) -> T { return x }

func testInfix9<T: P where T.T: Fooable>(x: T) {
  x.foo()#^INFIX_9^#
}
// FOOABLE_INFIX: Decl[InfixOperatorFunction]/CurrModule:   **** {#T.T#}[#T.T#]

func testInfix10<T: P where T.T: Fooable>(x: T) {
  (x.foo() **** x.foo())#^INFIX_10^#
}

func testInfix11() {
  S2#^INFIX_11^#
}

// INFIX_11: Begin completions, 2 items
// INFIX_11-DAG: Decl[Constructor]/CurrNominal:      ()[#S2#]; name=()
// INFIX_11-DAG: Keyword[self]/CurrNominal:          .self[#S2.Type#]; name=self
// INFIX_11: End completions

func testInfix12() {
  P#^INFIX_12^#
}
// INFIX_12: Begin completions, 3 items
// INFIX_12-NEXT: Decl[AssociatedType]/CurrNominal:   .T; name=T
// INFIX_12-NEXT: Decl[InstanceMethod]/CurrNominal:   .foo({#self: P#})[#() -> P.T#]; name=foo(P)
// INFIX_12-NEXT: Keyword[self]/CurrNominal:          .self[#P.Protocol#]; name=self
// INFIX_12: End completions

func testInfix13() {
  P.foo#^INFIX_13^#
}
// NO_OPERATORS-NOT: Decl[InfixOperatorFunction]

func testInfix14() {
  P.T#^INFIX_14^#
}
func testInfix15<T: P where T.T == S2>() {
  T#^INFIX_15^#
}
// INFIX_15: Begin completions, 3 items
// INFIX_15-NEXT: Decl[AssociatedType]/Super:         .T; name=T
// INFIX_15-NEXT: Decl[InstanceMethod]/Super:         .foo({#self: P#})[#() -> S2#]; name=foo(P)
// INFIX_15-NEXT: Keyword[self]/CurrNominal:          .self[#T.Type#]; name=self
// INFIX_15: End completions

func testInfix16<T: P where T.T == S2>() {
  T.foo#^INFIX_16^#
}

// INFIX_16: Begin completions, 2 items
// INFIX_16-NEXT: Pattern/CurrModule:               ({#(self): T#})[#() -> S2#]; name=(self: T)
// INFIX_16-NEXT: Keyword[self]/CurrNominal:         .self[#(T) -> () -> S2#]; name=self
// INFIX_16: End completions

func testInfix17(x: Void) {
  x#^INFIX_17^#
}

// VOID_OPERATORS: Begin completions
// VOID_OPERATORS-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  != {#()#}[#Bool#]; name=!= ()
// VOID_OPERATORS-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  == {#()#}[#Bool#]; name=== ()
// VOID_OPERATORS-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  <= {#()#}[#Bool#]; name=<= ()
// VOID_OPERATORS-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  >= {#()#}[#Bool#]; name=>= ()
// VOID_OPERATORS-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  < {#()#}[#Bool#]; name=< ()
// VOID_OPERATORS-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  > {#()#}[#Bool#]; name=> ()
// VOID_OPERATORS: End completions

func testInfix18(x: (S2, S2) {
  x#^INFIX_18^#
}
class EmptyClass {}
func testInfix19(x: EmptyClass) {
  x#^INFIX_19^#
}

// EMPTYCLASS_INFIX: Begin completions
// EMPTYCLASS_INFIX-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]: === {#AnyObject?#}[#Bool#]
// EMPTYCLASS_INFIX-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]: !== {#AnyObject?#}[#Bool#]
// EMPTYCLASS_INFIX: End completions

enum E {
  case A
  case B(S2)
}
func testInfix20(x: E) {
  x#^INFIX_20^#
}
func testInfix21() {
  E.A#^INFIX_21^#
}
func testInfix22() {
  E.B#^INFIX_22^#
}
// INFIX_22: Begin completions, 2 items
// INFIX_22-NEXT: Pattern/CurrModule:               ({#S2#})[#E#]; name=(S2)
// INFIX_22: End completions

func testSpace(x: S2) {
  x #^S2_INFIX_SPACE^#
}
// S2_INFIX_SPACE: Begin completions
// S2_INFIX_SPACE-DAG: Decl[InfixOperatorFunction]/CurrModule: [' ']** {#Int#}[#S2#]
// S2_INFIX_SPACE-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]: [' ']+ {#S2#}[#S2#]
// S2_INFIX_SPACE: End completions

func testExtInfix1(x: inout S2) {
  x + S2() + x + S2() + x + S2() + x#^EXT_INFIX_1^#
}

struct S4 {}
func +(x: S4, y: S4) -> S4 { return x }
func ==(x: S4, y: S4) -> Bool { return true }

infix operator +++ : ReallyLowPrecedence
precedencegroup ReallyLowPrecedence {
  associativity: left
  lowerThan: AssignmentPrecedence
}
func +++(x: S4, y: S4) -> S4 { return x }
infix operator &&& : ReallyHighPrecedence
precedencegroup ReallyHighPrecedence {
  associativity: left
  higherThan: BitwiseShiftPrecedence
}
func &&&(x: Bool, y: Bool) -> S4 { return x }

func testExtInfix2(x: S4) {
  x + x == x + x#^EXT_INFIX_2^#
}
// S4_EXT_INFIX: Begin completions
// S4_EXT_INFIX-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  != {#Bool#}[#Bool#]
// S4_EXT_INFIX-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  + {#S4#}[#S4#]
// S4_EXT_INFIX-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  == {#Bool#}[#Bool#]
// S4_EXT_INFIX-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  && {#Bool#}[#Bool#]
// S4_EXT_INFIX-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  || {#Bool#}[#Bool#]
// S4_EXT_INFIX: End completions

// S4_EXT_INFIX_NEG-NOT: +++
// S4_EXT_INFIX_NEG-NOT: &&&

func testExtInfix3(x: S4) {
   x + x#^EXT_INFIX_3^#
}
// S4_EXT_INFIX_SIMPLE: Begin completions
// S4_EXT_INFIX_SIMPLE-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]:  + {#S4#}[#S4#]
// S4_EXT_INFIX_SIMPLE-DAG: Decl[InfixOperatorFunction]/CurrModule:  +++ {#S4#}[#S4#]
// S4_EXT_INFIX_SIMPLE: End completions

func testExtInfix4(x: S4) {
   1 + 1.0 + x#^EXT_INFIX_4^#
}

func testAssignTuple1() {
  ()#^ASSIGN_TUPLE_1^#
}
func testAssignTuple3() {
  func void() {}
  void()#^ASSIGN_TUPLE_3^#
}
// FIXME: technically this is sometimes legal, but we would need to
// differentiate between cases like () = and print() =. Since it's not very
// useful anyway, just omit the completion.
// ASSIGN_TUPLE_1-NOT: BuiltinOperator/None:  = {

func testAssignTuple2() {
  var x: S2
  var y: S2
  (x, y)#^ASSIGN_TUPLE_2^#
}
// ASSIGN_TUPLE_2: BuiltinOperator/None:                        = {#(S2, S2)#}[#Void#];


infix operator ====: ComparisonPrecedence
infix operator &&&& : LogicalConjunctionPrecedence
infix operator |||| : LogicalDisjunctionPrecedence
struct Boolish {}
func ====(x: Boolish, y: Boolish) -> Boolish { return x }
func &&&&(x: Boolish, y: @autoclosure ()->Boolish) -> Boolish { return x }
func ||||(x: Boolish, y: @autoclosure ()->Boolish) -> Boolish { return x }

func testAutoclosure(x: Boolish, y: Boolish) {
  if x #^INFIX_AUTOCLOSURE_1^# {}
  if x &&&& y #^INFIX_AUTOCLOSURE_2^# {}
  if x |||| y #^INFIX_AUTOCLOSURE_3^# {}
  if x &&&& x |||| y #^INFIX_AUTOCLOSURE_4^# {}
}

// INFIX_AUTOCLOSURE_1: Begin completions
// INFIX_AUTOCLOSURE_1-DAG: Decl[InfixOperatorFunction]/CurrModule: [' ']&&&& {#Boolish#}[#Boolish#];
// INFIX_AUTOCLOSURE_1-DAG: Decl[InfixOperatorFunction]/CurrModule: [' ']==== {#Boolish#}[#Boolish#];
// INFIX_AUTOCLOSURE_1-DAG: Decl[InfixOperatorFunction]/CurrModule: [' ']|||| {#Boolish#}[#Boolish#];
// INFIX_AUTOCLOSURE_1: End completions