// RUN: %batch-code-completion

func testAll0() {
  // Not type context.
  let x = #^NO_CONTEXT_0^#
// NO_CONTEXT_0-DAG: Literal[Integer]/None:              0[#Int#];
// NO_CONTEXT_0-DAG: Literal[Boolean]/None:              true[#Bool#];
// NO_CONTEXT_0-DAG: Literal[Boolean]/None:              false[#Bool#];
// NO_CONTEXT_0-DAG: Literal[Nil]/None:                  nil;
// NO_CONTEXT_0-DAG: Literal[String]/None:               "{#(abc)#}"[#String#];
// NO_CONTEXT_0-DAG: Literal[Array]/None:                [{#(values)#}][#Array<Element>#];
// NO_CONTEXT_0-DAG: Literal[Dictionary]/None:           [{#(key)#}: {#(value)#}][#Dictionary<Key, Value>#];
// NO_CONTEXT_0-DAG: Literal[_Color]/None:               #colorLiteral({#red: Float#}, {#green: Float#}, {#blue: Float#}, {#alpha: Float#});
// NO_CONTEXT_0-DAG: Literal[_Image]/None:               #imageLiteral({#resourceName: String#});
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
struct MyUnicodeScalar1: ExpressibleByUnicodeScalarLiteral {
  init(unicodeScalarLiteral value: Character) {}
}
struct MyCharacter1: ExpressibleByExtendedGraphemeClusterLiteral {
  init(unicodeScalarLiteral value: Character) {}
  init(extendedGraphemeClusterLiteral value: String) {}
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
// NIL_1: Literal[Nil]/None/TypeRelation[Convertible]: nil[#MyNil1#];

func testNil2() {
  let x: Int? = #^NIL_2^#
}
// NIL_2: Literal[Nil]/None/TypeRelation[Convertible]: nil[#Int?#];

func testBool0() {
  let x: Int = #^BOOL_0^#
}
// BOOL_0: Literal[Boolean]/None: true[#Bool#];
// BOOL_0: Literal[Boolean]/None: false[#Bool#];

func testBool1() {
  let x: MyBool1 = #^BOOL_1^#
}
// BOOL_1: Literal[Boolean]/None/TypeRelation[Convertible]: true[#MyBool1#];
// BOOL_1: Literal[Boolean]/None/TypeRelation[Convertible]: false[#MyBool1#];

func testBool2() {
  let x: Bool = #^BOOL_2^#
}
// BOOL_2: Literal[Boolean]/None/TypeRelation[Convertible]: true[#Bool#];
// BOOL_2: Literal[Boolean]/None/TypeRelation[Convertible]: false[#Bool#];

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
// INT_1: Literal[Integer]/None/TypeRelation[Convertible]: 0[#MyInt1#];

func testInt2() {
  let x: Int = #^INT_2^#
}
// INT_2: Literal[Integer]/None/TypeRelation[Convertible]: 0[#Int#];

func testDouble0() {
  let x: Double = #^DOUBLE_0^#
}
// DOUBLE_0: Literal[Integer]/None/TypeRelation[Convertible]: 0[#Double#];

func testString0() {
  let x: Int = #^STRING_0^#
}
// STRING_0: Literal[String]/None: "{#(abc)#}"[#String#];

func testString1() {
  let x: MyString1 = #^STRING_1^#
}
// STRING_1: Literal[String]/None/TypeRelation[Convertible]: "{#(abc)#}"[#MyString1#];

func testString2() {
  let x: String = #^STRING_2^#
}
// STRING_2: Literal[String]/None/TypeRelation[Convertible]: "{#(abc)#}"[#String#];

func testString3() {
  let x: MyUnicodeScalar1 = #^STRING_3^#
}
// STRING_3: Literal[String]/None/TypeRelation[Convertible]: "{#(abc)#}"[#MyUnicodeScalar1#];
func testString4() {
  let x: MyCharacter1 = #^STRING_4^#
}
// STRING_4: Literal[String]/None/TypeRelation[Convertible]: "{#(abc)#}"[#MyCharacter1#];
func testString5() {
  let x: Character = #^STRING_5^#
}
// STRING_5: Literal[String]/None/TypeRelation[Convertible]: "{#(abc)#}"[#Character#];

func testArray0() {
  let x: Int = #^ARRAY_0^#
}
// ARRAY_0: Literal[Array]/None: [{#(values)#}][#Array<Element>#];

func testArray1() {
  let x: MyArray1<MyInt1> = #^ARRAY_1^#
}
// ARRAY_1: Literal[Array]/None/TypeRelation[Convertible]: [{#(values)#}][#MyArray1<MyInt1>#];

func testArray2() {
  let x: [MyInt1] = #^ARRAY_2^#
}
// ARRAY_2: Literal[Array]/None/TypeRelation[Convertible]: [{#(values)#}][#[MyInt1]#];

func testDict0() {
  let x: Int = #^DICT_0^#
}
// DICT_0: Literal[Dictionary]/None: [{#(key)#}: {#(value)#}][#Dictionary<Key, Value>#];

func testDict1() {
  let x: MyDict1<MyInt1, MyString1> = #^DICT_1^#
}
// DICT_1: Literal[Dictionary]/None/TypeRelation[Convertible]: [{#(key)#}: {#(value)#}][#MyDict1<MyInt1, MyString1>#];

func testDict2() {
  let x: [MyInt1: MyString1] = #^DICT_2^#
}
// DICT_2: Literal[Dictionary]/None/TypeRelation[Convertible]: [{#(key)#}: {#(value)#}][#[MyInt1 : MyString1]#];

func testTuple0() {
  let x: Int = #^TUPLE_0^#
}
// TUPLE_0: Literal[Tuple]/None: ({#(values)#});

func testTuple1() {
  let x: (MyInt1, MyString1) = #^TUPLE_1^#
}
// TUPLE_1: Literal[Tuple]/None/TypeRelation[Convertible]: ({#(values)#})[#(MyInt1, MyString1)#];

func testTuple2() {
  let x: (MyInt1, MyString1, MyDouble1) = #^TUPLE_2^#
}
// FIXME: should we extend the tuple to have the right number of elements?
// TUPLE_2: Literal[Tuple]/None/TypeRelation[Convertible]: ({#(values)#})[#(MyInt1, MyString1, MyDouble1)#];

struct MyColor1: _ExpressibleByColorLiteral {
  init(_colorLiteralRed: Float, green: Float, blue: Float, alpha: Float) {}
}
func testColor0() {
  let x: Int = #^COLOR_0^#
}
// COLOR_0: Literal[_Color]/None: #colorLiteral({#red: Float#}, {#green: Float#}, {#blue: Float#}, {#alpha: Float#});

func testColor1() {
  let x: MyColor1 = #^COLOR_1^#
}
// COLOR_1: Literal[_Color]/None/TypeRelation[Convertible]: #colorLiteral({#red: Float#}, {#green: Float#}, {#blue: Float#}, {#alpha: Float#})[#MyColor1#];

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
// IMAGE_1: Literal[_Image]/None/TypeRelation[Convertible]: #imageLiteral({#resourceName: String#})[#MyImage1#];
