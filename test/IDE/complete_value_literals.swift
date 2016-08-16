// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=NO_CONTEXT_0 | %FileCheck %s -check-prefix=NO_CONTEXT_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=NIL_0 | %FileCheck %s -check-prefix=NIL_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=NIL_1 | %FileCheck %s -check-prefix=NIL_1
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=NIL_2 | %FileCheck %s -check-prefix=NIL_2
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=BOOL_0 | %FileCheck %s -check-prefix=BOOL_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=BOOL_1 | %FileCheck %s -check-prefix=BOOL_1
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=BOOL_2 | %FileCheck %s -check-prefix=BOOL_2
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=BOOL_3 | %FileCheck %s -check-prefix=BOOL_3
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=BOOL_4 | %FileCheck %s -check-prefix=BOOL_4
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INT_0 | %FileCheck %s -check-prefix=INT_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INT_1 | %FileCheck %s -check-prefix=INT_1
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=INT_2 | %FileCheck %s -check-prefix=INT_2
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=DOUBLE_0 | %FileCheck %s -check-prefix=DOUBLE_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=STRING_0 | %FileCheck %s -check-prefix=STRING_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=STRING_1 | %FileCheck %s -check-prefix=STRING_1
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=STRING_2 | %FileCheck %s -check-prefix=STRING_2
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=ARRAY_0 | %FileCheck %s -check-prefix=ARRAY_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=ARRAY_1 | %FileCheck %s -check-prefix=ARRAY_1
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=ARRAY_2 | %FileCheck %s -check-prefix=ARRAY_2
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=DICT_0 | %FileCheck %s -check-prefix=DICT_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=DICT_1 | %FileCheck %s -check-prefix=DICT_1
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=DICT_2 | %FileCheck %s -check-prefix=DICT_2
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=TUPLE_0 | %FileCheck %s -check-prefix=TUPLE_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=TUPLE_1 | %FileCheck %s -check-prefix=TUPLE_1
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=TUPLE_2 | %FileCheck %s -check-prefix=TUPLE_2
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=COLOR_0 | %FileCheck %s -check-prefix=COLOR_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=COLOR_1 | %FileCheck %s -check-prefix=COLOR_1
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=COLOR_2 | %FileCheck %s -check-prefix=COLOR_2
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=IMAGE_0 | %FileCheck %s -check-prefix=IMAGE_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=IMAGE_1 | %FileCheck %s -check-prefix=IMAGE_1

func testAll0() {
  // Not type context.
  let x = #^NO_CONTEXT_0^#
// NO_CONTEXT_0-DAG: Begin completions
// NO_CONTEXT_0-DAG: Literal[Integer]/None:              0[#Int#];
// NO_CONTEXT_0-DAG: Literal[Boolean]/None:              true[#Bool#];
// NO_CONTEXT_0-DAG: Literal[Boolean]/None:              false[#Bool#];
// NO_CONTEXT_0-DAG: Literal[Nil]/None:                  nil;
// NO_CONTEXT_0-DAG: Literal[String]/None:               "{#(abc)#}"[#String#];
// NO_CONTEXT_0-DAG: Literal[Array]/None:                [{#(values)#}][#Array#];
// NO_CONTEXT_0-DAG: Literal[Dictionary]/None:           [{#(key)#}: {#(value)#}][#Dictionary#];
// NO_CONTEXT_0-DAG: Literal[_Color]/None:               #colorLiteral({#red: Float#}, {#green: Float#}, {#blue: Float#}, {#alpha: Float#});
// NO_CONTEXT_0-DAG: Literal[_Image]/None:               #imageLiteral({#resourceName: String#});
// NO_CONTEXT_0: End completions
}

struct MyNil1: ExpressibleByNilLiteral {
  init(nilLiteral: ()) {}
}
struct MyBool1: ExpressibleByBooleanLiteral {
  init(booleanLiteral value: Bool) {}
}
struct MyInt1: ExpressibleByIntegerLiteral {
  init(integerLiteral value: Int) {}
}
struct MyDouble1: ExpressibleByFloatLiteral {
  init(floatLiteral value: Double) {}
}
struct MyString1: ExpressibleByStringLiteral {
  init(unicodeScalarLiteral value: Character) {}
  init(extendedGraphemeClusterLiteral value: String) {}
  init(stringLiteral value: String) {}
}
struct MyArray1<Element>: ExpressibleByArrayLiteral {
  init(arrayLiteral value: Element...) {}
}
struct MyDict1<Key, Value>: ExpressibleByDictionaryLiteral {
  init(dictionaryLiteral elements: (Key, Value)...) {}
}

extension MyInt1: Equatable, Hashable {
  var hashValue: Int { return 1 }
}
func ==(_: MyInt1, _: MyInt1) -> Bool { return true }

func testNil0() {
  let x: Int = #^NIL_0^#
}
// NIL_0: Literal[Nil]/None: nil;

func testNil1() {
  let x: MyNil1 = #^NIL_1^#
}
// NIL_1: Literal[Nil]/None/TypeRelation[Identical]: nil[#MyNil1#];

func testNil2() {
  let x: Int? = #^NIL_2^#
}
// NIL_2: Literal[Nil]/None/TypeRelation[Identical]: nil[#Int?#];

func testBool0() {
  let x: Int = #^BOOL_0^#
}
// BOOL_0: Literal[Boolean]/None: true[#Bool#];
// BOOL_0: Literal[Boolean]/None: false[#Bool#];

func testBool1() {
  let x: MyBool1 = #^BOOL_1^#
}
// BOOL_1: Literal[Boolean]/None/TypeRelation[Identical]: true[#MyBool1#];
// BOOL_1: Literal[Boolean]/None/TypeRelation[Identical]: false[#MyBool1#];

func testBool2() {
  let x: Bool = #^BOOL_2^#
}
// BOOL_2: Literal[Boolean]/None/TypeRelation[Identical]: true[#Bool#];
// BOOL_2: Literal[Boolean]/None/TypeRelation[Identical]: false[#Bool#];

func testBool3() {
  let x: Bool? = #^BOOL_3^#
}
// BOOL_3: Literal[Boolean]/None/TypeRelation[Convertible]: true[#Bool#];
// BOOL_3: Literal[Boolean]/None/TypeRelation[Convertible]: false[#Bool#];

func testBool4() {
  let x: Bool! = #^BOOL_4^#
}
// BOOL_4: Literal[Boolean]/None/TypeRelation[Convertible]: true[#Bool#];
// BOOL_4: Literal[Boolean]/None/TypeRelation[Convertible]: false[#Bool#];

func testInt0() {
  let x: Bool = #^INT_0^#
}
// INT_0: Literal[Integer]/None: 0[#Int#];

func testInt1() {
  let x: MyInt1 = #^INT_1^#
}
// INT_1: Literal[Integer]/None/TypeRelation[Identical]: 0[#MyInt1#];

func testInt2() {
  let x: Int = #^INT_2^#
}
// INT_2: Literal[Integer]/None/TypeRelation[Identical]: 0[#Int#];

func testDouble0() {
  let x: Double = #^DOUBLE_0^#
}
// DOUBLE_0: Literal[Integer]/None/TypeRelation[Identical]: 0[#Double#];

func testString0() {
  let x: Int = #^STRING_0^#
}
// STRING_0: Literal[String]/None: "{#(abc)#}"[#String#];

func testString1() {
  let x: MyString1 = #^STRING_1^#
}
// STRING_1: Literal[String]/None/TypeRelation[Identical]: "{#(abc)#}"[#MyString1#];

func testString2() {
  let x: String = #^STRING_2^#
}
// STRING_2: Literal[String]/None/TypeRelation[Identical]: "{#(abc)#}"[#String#];

func testArray0() {
  let x: Int = #^ARRAY_0^#
}
// ARRAY_0: Literal[Array]/None: [{#(values)#}][#Array#];

func testArray1() {
  let x: MyArray1<MyInt1> = #^ARRAY_1^#
}
// ARRAY_1: Literal[Array]/None/TypeRelation[Identical]: [{#(values)#}][#MyArray1<MyInt1>#];

func testArray2() {
  let x: [MyInt1] = #^ARRAY_2^#
}
// ARRAY_2: Literal[Array]/None/TypeRelation[Identical]: [{#(values)#}][#[MyInt1]#];

func testDict0() {
  let x: Int = #^DICT_0^#
}
// DICT_0: Literal[Dictionary]/None: [{#(key)#}: {#(value)#}][#Dictionary#];

func testDict1() {
  let x: MyDict1<MyInt1, MyString1> = #^DICT_1^#
}
// DICT_1: Literal[Dictionary]/None/TypeRelation[Identical]: [{#(key)#}: {#(value)#}][#MyDict1<MyInt1, MyString1>#];

func testDict2() {
  let x: [MyInt1: MyString1] = #^DICT_2^#
}
// DICT_2: Literal[Dictionary]/None/TypeRelation[Identical]: [{#(key)#}: {#(value)#}][#[MyInt1 : MyString1]#];

func testTuple0() {
  let x: Int = #^TUPLE_0^#
}
// TUPLE_0: Literal[Tuple]/None: ({#(values)#});

func testTuple1() {
  let x: (MyInt1, MyString1) = #^TUPLE_1^#
}
// TUPLE_1: Literal[Tuple]/None/TypeRelation[Identical]: ({#(values)#})[#(MyInt1, MyString1)#];

func testTuple2() {
  let x: (MyInt1, MyString1, MyDouble1) = #^TUPLE_2^#
}
// FIXME: should we extend the tuple to have the right number of elements?
// TUPLE_2: Literal[Tuple]/None/TypeRelation[Identical]: ({#(values)#})[#(MyInt1, MyString1, MyDouble1)#];

struct MyColor1: _ExpressibleByColorLiteral {
  init(colorLiteralRed: Float, green: Float, blue: Float, alpha: Float) {}
}
func testColor0() {
  let x: Int = #^COLOR_0^#
}
// COLOR_0: Literal[_Color]/None: #colorLiteral({#red: Float#}, {#green: Float#}, {#blue: Float#}, {#alpha: Float#});

func testColor1() {
  let x: MyColor1 = #^COLOR_1^#
}
// COLOR_1: Literal[_Color]/None/TypeRelation[Identical]: #colorLiteral({#red: Float#}, {#green: Float#}, {#blue: Float#}, {#alpha: Float#})[#MyColor1#];

func testColor2() {
  let x: MyColor1? = #^COLOR_2^#
}
// COLOR_2: Literal[_Color]/None/TypeRelation[Convertible]: #colorLiteral({#red: Float#}, {#green: Float#}, {#blue: Float#}, {#alpha: Float#})[#MyColor1#];

struct MyImage1: _ExpressibleByImageLiteral {
  init(imageLiteralResourceName: String) {}
}
func testImage0() {
  let x: Int = #^IMAGE_0^#
}
// IMAGE_0: Literal[_Image]/None: #imageLiteral({#resourceName: String#});

func testImage1() {
  let x: MyImage1 = #^IMAGE_1^#
}
// IMAGE_1: Literal[_Image]/None/TypeRelation[Identical]: #imageLiteral({#resourceName: String#})[#MyImage1#];
