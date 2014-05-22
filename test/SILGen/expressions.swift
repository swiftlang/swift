// RUN: %swift -parse-stdlib -emit-silgen %s -enable-character-literals | FileCheck %s
import Swift

typealias CharacterLiteralType = SillyCharacter

struct SillyCharacter :
    _BuiltinCharacterLiteralConvertible, CharacterLiteralConvertible {

  static func _convertFromBuiltinCharacterLiteral(
    value: Builtin.Int32
  ) -> SillyCharacter {
    return SillyCharacter()
  }

  static func convertFromCharacterLiteral(value: SillyCharacter) -> SillyCharacter {
    return value
  }
}

struct SillyString : _BuiltinStringLiteralConvertible, StringLiteralConvertible {
  static func _convertFromBuiltinExtendedGraphemeClusterLiteral(
    start: Builtin.RawPointer,
    byteSize: Builtin.Word,
    isASCII: Builtin.Int1) -> SillyString { }

  static func convertFromExtendedGraphemeClusterLiteral(
    value: SillyString) -> SillyString { }

  static func _convertFromBuiltinStringLiteral(start: Builtin.RawPointer,
                                               byteSize: Builtin.Word,
                                               isASCII: Builtin.Int1) -> SillyString { }
  static func convertFromStringLiteral(value: SillyString) -> SillyString { }
}

struct SillyUTF16String : _BuiltinUTF16StringLiteralConvertible, StringLiteralConvertible {
  static func _convertFromBuiltinExtendedGraphemeClusterLiteral(
    start: Builtin.RawPointer,
    byteSize: Builtin.Word,
    isASCII: Builtin.Int1) -> SillyUTF16String { }

  static func convertFromExtendedGraphemeClusterLiteral(
    value: SillyString) -> SillyUTF16String { }

  static func _convertFromBuiltinStringLiteral(start: Builtin.RawPointer,
                                               byteSize: Builtin.Word,
                                               isASCII: Builtin.Int1) -> SillyUTF16String { }

  static func _convertFromBuiltinUTF16StringLiteral(start: Builtin.RawPointer,
                                                    numberOfCodeUnits: Builtin.Word) -> SillyUTF16String { }
  static func convertFromStringLiteral(value: SillyUTF16String) -> SillyUTF16String { }
}

func literals() {
  var a = 1
  var b = 1.25
  var c = 'x'
  var d = "foo"
  var e:SillyString = "foo"
}
// CHECK-LABEL: sil  @_TF11expressions8literalsFT_T_
// CHECK: integer_literal $Builtin.Int2048, 1
// CHECK: float_literal $Builtin.FPIEEE64, 0x3FF4000000000000
// CHECK: integer_literal $Builtin.Int32, 120
// CHECK: string_literal utf16 "foo"
// CHECK: string_literal utf8 "foo"

func bar(x: Int) {}
func bar(x: Int, y: Int) {}

func call_one() {
  bar(42);
}

// CHECK-LABEL: sil  @_TF11expressions8call_oneFT_T_
// CHECK: [[BAR:%[0-9]+]] = function_ref @_TF11expressions3bar{{.*}} : $@thin (Int) -> ()
// CHECK: [[FORTYTWO:%[0-9]+]] = integer_literal {{.*}} 42
// CHECK: [[FORTYTWO_CONVERTED:%[0-9]+]] = apply {{.*}}([[FORTYTWO]], {{.*}})
// CHECK: apply [[BAR]]([[FORTYTWO_CONVERTED]])

func call_two() {
  bar(42, 219)
}

// CHECK-LABEL: sil  @_TF11expressions8call_twoFT_T_
// CHECK: [[BAR:%[0-9]+]] = function_ref @_TF11expressions3bar{{.*}} : $@thin (Int, Int) -> ()
// CHECK: [[FORTYTWO:%[0-9]+]] = integer_literal {{.*}} 42
// CHECK: [[FORTYTWO_CONVERTED:%[0-9]+]] = apply {{.*}}([[FORTYTWO]], {{.*}})
// CHECK: [[TWONINETEEN:%[0-9]+]] = integer_literal {{.*}} 219
// CHECK: [[TWONINETEEN_CONVERTED:%[0-9]+]] = apply {{.*}}([[TWONINETEEN]], {{.*}})
// CHECK: apply [[BAR]]([[FORTYTWO_CONVERTED]], [[TWONINETEEN_CONVERTED]])

func tuples() {
  bar((4, 5).1)

  var T1 : (a: Int16, b: Int) = (b : 42, a : 777)

  // varargs.
  var va1 : (Int...) = ()
  var va2 : (Int, Int...) = (1,2,3)
}

// CHECK-LABEL: sil  @_TF11expressions6tuplesFT_T_


class C {
  var chi:Int
  init() {
    chi = 219
  }
  init(x:Int) {
    chi = x
  }
}

// CHECK-LABEL: sil  @_TF11expressions7classesFT_T_
func classes() {
  // CHECK: function_ref @_TFC11expressions1CCfMS0_FT_S0_ : $@thin (@thick C.Type) -> @owned C
  var a = C()
  // CHECK: function_ref @_TFC11expressions1CCfMS0_FT1xSi_S0_ : $@thin (Int, @thick C.Type) -> @owned C
  var b = C(x: 0)
}

struct S {
  var x:Int
  init() {
    x = 219
  }
  init(x: Int) {
    self.x = x
  }
}

// CHECK-LABEL: sil  @_TF11expressions7structsFT_T_
func structs() {
  // CHECK: function_ref @_TFV11expressions1SCfMS0_FT_S0_ : $@thin (@thin S.Type) -> S
  var a = S()
  // CHECK: function_ref @_TFV11expressions1SCfMS0_FT1xSi_S0_ : $@thin (Int, @thin S.Type) -> S
  var b = S(x: 0)
}


func inoutcallee(inout x: Int) {}
func address_of_expr() {
  var x: Int = 4
  inoutcallee(&x)
}



func identity<T>(x: T) -> T {}

// CHECK-LABEL: sil  @_TF11expressions15scalar_to_tupleFT_T_
func scalar_to_tuple() {
  var a : (x: Int) = 42
  var b : (Int...) = 42
  var c : (Int, Int...) = 42
}

// CHECK-LABEL: sil  @_TF11expressions11array_alloc
func array_alloc(n: Int) {
  var a: Int[] = new Int[n]
}

struct SomeStruct {
 mutating
  func a() {}
}

// CHECK-LABEL: sil  @_TF11expressions5callsFT_T_
// CHECK: [[METHOD:%[0-9]+]] = function_ref @_TFV11expressions10SomeStruct1afRS0_FT_T_ : $@cc(method) @thin (@inout SomeStruct) -> ()
// CHECK: apply [[METHOD]]({{.*}})
func calls() {
  var a : SomeStruct
  a.a()
}

// CHECK-LABEL: sil  @_TF11expressions11module_pathFT_Sb
func module_path() -> Bool {
  return Swift.true
  // CHECK: [[TRUE_GET:%[0-9]+]] = function_ref @_TFSsg4trueSb
  // CHECK: apply [transparent] [[TRUE_GET]]()
}

func default_args(x: Int, y: Int = 219, z: Int = 20721) {}

// CHECK-LABEL: sil  @_TF11expressions19call_default_args_1
func call_default_args_1(x: Int) {
  default_args(x)
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF11expressions12default_args
  // CHECK: [[YFUNC:%[0-9]+]] = function_ref @_TIF11expressions12default_args
  // CHECK: [[Y:%[0-9]+]] = apply [[YFUNC]]()
  // CHECK: [[ZFUNC:%[0-9]+]] = function_ref @_TIF11expressions12default_args
  // CHECK: [[Z:%[0-9]+]] = apply [[ZFUNC]]()
  // CHECK: apply [[FUNC]]({{.*}}, [[Y]], [[Z]])
}

// CHECK-LABEL: sil  @_TF11expressions19call_default_args_2
func call_default_args_2(x: Int, z: Int) {
  default_args(x, z:z)
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF11expressions12default_args
  // CHECK: [[DEFFN:%[0-9]+]] = function_ref @_TIF11expressions12default_args
  // CHECK-NEXT: [[C219:%[0-9]+]] = apply [[DEFFN]]()
  // CHECK: apply [[FUNC]]({{.*}}, [[C219]], {{.*}})
}

struct Generic<T> {
  var mono_member:Int
  var typevar_member:T

  // CHECK-LABEL: sil  @_TFV11expressions7Generic13type_variable
  mutating
  func type_variable() -> T.Type {
    return T.self
    // CHECK: [[METATYPE:%[0-9]+]] = metatype $@thick T.Type
    // CHECK: return [[METATYPE]]
  }

  // CHECK-LABEL: sil  @_TFV11expressions7Generic19copy_typevar_member
  mutating
  func copy_typevar_member(x: Generic<T>) {
    typevar_member = x.typevar_member
  }

  // CHECK-LABEL: sil  @_TFV11expressions7Generic12class_methodU__fMGS0_Q__FT_T_
  static func class_method() {}
}

// CHECK-LABEL: sil  @_TF11expressions18generic_member_ref
func generic_member_ref<T>(x: Generic<T>) -> Int {
  // CHECK: bb0([[XADDR:%[0-9]+]] : $*Generic<T>):
  return x.mono_member
  // CHECK: [[MEMBER_ADDR:%[0-9]+]] = struct_element_addr {{.*}}, #Generic.mono_member
  // CHECK: load [[MEMBER_ADDR]]
}

// CHECK-LABEL: sil  @_TF11expressions24bound_generic_member_ref
func bound_generic_member_ref(var x: Generic<UnicodeScalar>) -> Int {
  // CHECK: bb0([[XADDR:%[0-9]+]] : $Generic<UnicodeScalar>):
  return x.mono_member
  // CHECK: [[MEMBER_ADDR:%[0-9]+]] = struct_element_addr {{.*}}, #Generic.mono_member
  // CHECK: load [[MEMBER_ADDR]]
}

// CHECK-LABEL: sil  @_TF11expressions6coerce
func coerce(x: Int32) -> Int64 {
  return 0
}

class B {
}

class D : B {
}

// CHECK-LABEL: sil  @_TF11expressions8downcast
func downcast(x: B) -> D {
  return x as D
  // CHECK: unconditional_checked_cast downcast %{{[0-9]+}} : {{.*}} to $D
}

// CHECK-LABEL: sil  @_TF11expressions6upcast
func upcast(x: D) -> B {
  return x
  // CHECK: upcast %{{[0-9]+}} : ${{.*}} to $B
}

// CHECK-LABEL: sil  @_TF11expressions14generic_upcast
func generic_upcast<T : B>(x: T) -> B {
  return x
  // CHECK: upcast %{{.*}} to $B
  // CHECK: return
}

// CHECK-LABEL: sil  @_TF11expressions16generic_downcast
func generic_downcast<T : B>(x: T, y: B) -> T {
  return y as T
  // CHECK: unconditional_checked_cast super_to_archetype %{{[0-9]+}} : {{.*}} to $T
  // CHECK: return
}

// TODO: generic_downcast

// CHECK-LABEL: sil  @_TF11expressions15metatype_upcast
func metatype_upcast() -> B.Type {
  return D.self
  // CHECK: metatype $@thick D
  // CHECK-NEXT: upcast
}

// CHECK-LABEL: sil  @_TF11expressions19interpolated_string
func interpolated_string(x: Int, y: String) -> String {
  return "The \(x) Million Dollar \(y)"
}

protocol Runcible {
  typealias U
  var free:Int { get }
  var associated:U { get }

  func free_method() -> Int
  mutating
  func associated_method() -> U.Type
  class func static_method()
}

protocol Bendable { }
protocol Wibbleable { }

// CHECK-LABEL: sil  @_TF11expressions20archetype_member_ref
func archetype_member_ref<T : Runcible>(var x: T) {
  x.free_method()
  // CHECK: witness_method
  // CHECK-NEXT: apply
  var u = x.associated_method()
  T.static_method()
}

// CHECK-LABEL: sil  @_TF11expressions22existential_member_ref
func existential_member_ref(x: Runcible) {
  x.free_method()
  // CHECK: project_existential
  // CHECK-NEXT: protocol_method
  // CHECK-NEXT: apply
}

/*TODO archetype and existential properties and subscripts
func archetype_property_ref<T : Runcible>(x: T) -> (Int, T.U) {
  x.free = x.free_method()
  x.associated = x.associated_method()
  return (x.free, x.associated)
}

func existential_property_ref<T : Runcible>(x: T) -> Int {
  x.free = x.free_method()
  return x.free
}

also archetype/existential subscripts
*/

struct Spoon : Runcible {
  typealias U = Float
  var free: Int { return 4 }
  var associated: Float { return 12 }

  func free_method() -> Int {}
  func associated_method() -> Float.Type {}
  static func static_method() {}
}

struct Hat<T> : Runcible {
  typealias U = T[]
  var free: Int { return 1 }
  var associated: U { get {} }

  func free_method() -> Int {}

  // CHECK-LABEL: sil  @_TFV11expressions3Hat17associated_method
  mutating
  func associated_method() -> U.Type {
    return U.self
    // CHECK: [[META:%[0-9]+]] = metatype $@thin Array<T>.Type
    // CHECK: return [[META]]
  }

  static func static_method() {}
}

// CHECK-LABEL: sil  @_TF11expressions7erasure
func erasure(x: Spoon) -> Runcible {
  return x
  // CHECK: init_existential
  // CHECK: return
}

// CHECK-LABEL: sil  @_TF11expressions18erasure_from_proto
func erasure_from_proto(x: protocol<Wibbleable, Bendable>) -> Bendable {
  return x
  // CHECK: upcast_existential
  // CHECK: return
}

// CHECK-LABEL: sil  @_TF11expressions19declref_to_metatypeFT_MVS_5Spoon
func declref_to_metatype() -> Spoon.Type {
  return Spoon.self
  // CHECK: metatype $@thin Spoon.Type
}

// CHECK-LABEL: sil  @_TF11expressions27declref_to_generic_metatype
func declref_to_generic_metatype() -> Generic<UnicodeScalar>.Type {
  // FIXME parsing of T<U> in expression context
  typealias GenericChar = Generic<UnicodeScalar>
  return GenericChar.self
  // CHECK: metatype $@thin Generic<UnicodeScalar>.Type
}

func int(x: Int) {}
func float(x: Float) {}

func tuple() -> (Int, Float) { return (1, 1.0) }

// CHECK-LABEL: sil  @_TF11expressions13tuple_element
func tuple_element(var x: (Int, Float)) {
  // CHECK: [[XADDR:%.*]] = alloc_box $(Int, Float)

  int(x.0)
  // CHECK: tuple_element_addr [[XADDR]]#1 : {{.*}}, 0
  // CHECK: apply

  float(x.1)
  // CHECK: tuple_element_addr [[XADDR]]#1 : {{.*}}, 1
  // CHECK: apply

  int(tuple().0)
  // CHECK: [[ZERO:%.*]] = tuple_extract {{%.*}} : {{.*}}, 0
  // CHECK: apply {{.*}}([[ZERO]])

  float(tuple().1)
  // CHECK: [[ONE:%.*]] = tuple_extract {{%.*}} : {{.*}}, 1
  // CHECK: apply {{.*}}([[ONE]])
}

// CHECK-LABEL: sil  @_TF11expressions10containers
func containers() -> (Int[], Dictionary<String, Int>) {
  return ([1, 2, 3], ["Ankeny": 1, "Burnside": 2, "Couch": 3])
}

// CHECK-LABEL: sil  @_TF11expressions7if_expr
func if_expr(var a: Bool, var b: Bool, var x: Int, var y: Int, var z: Int) -> Int {
  // CHECK: bb0({{.*}}):
  // CHECK: [[AB:%[0-9]+]] = alloc_box $Bool
  // CHECK: [[BB:%[0-9]+]] = alloc_box $Bool
  // CHECK: [[XB:%[0-9]+]] = alloc_box $Int
  // CHECK: [[YB:%[0-9]+]] = alloc_box $Int
  // CHECK: [[ZB:%[0-9]+]] = alloc_box $Int

  return a
    ? x
    : b
    ? y
    : z
  // CHECK:   [[A:%[0-9]+]] = load [[AB]]#1
  // CHECK:   [[ACOND:%[0-9]+]] = apply {{.*}}([[A]])
  // CHECK:   cond_br [[ACOND]], [[IF_A:bb[0-9]+]], [[ELSE_A:bb[0-9]+]]
  // CHECK: [[IF_A]]:
  // CHECK:   [[XVAL:%[0-9]+]] = load [[XB]]
  // CHECK:   br [[CONT_A:bb[0-9]+]]([[XVAL]] : $Int)
  // CHECK: [[ELSE_A]]:
  // CHECK:   [[B:%[0-9]+]] = load [[BB]]#1
  // CHECK:   [[BCOND:%[0-9]+]] = apply {{.*}}([[B]])
  // CHECK:   cond_br [[BCOND]], [[IF_B:bb[0-9]+]], [[ELSE_B:bb[0-9]+]]
  // CHECK: [[IF_B]]:
  // CHECK:   [[YVAL:%[0-9]+]] = load [[YB]]
  // CHECK:   br [[CONT_B:bb[0-9]+]]([[YVAL]] : $Int)
  // CHECK: [[ELSE_B]]:
  // CHECK:   [[ZVAL:%[0-9]+]] = load [[ZB]]
  // CHECK:   br [[CONT_B:bb[0-9]+]]([[ZVAL]] : $Int)
  // CHECK: [[CONT_B]]([[B_RES:%[0-9]+]] : $Int):
  // CHECK:   br [[CONT_A:bb[0-9]+]]([[B_RES]] : $Int)
  // CHECK: [[CONT_A]]([[A_RES:%[0-9]+]] : $Int):
  // CHECK:   return [[A_RES]]
}


// Test that magic identifiers expand properly.  We test __COLUMN__ here because
// it isn't affected as this testcase slides up and down the file over time.
func magic_identifier_expansion(a: Int = __COLUMN__) {
  // CHECK-LABEL: sil  @{{.*}}magic_identifier_expansion
  
  // This should expand to the column number of the first _.
  var tmp = __COLUMN__
  // CHECK: integer_literal $Builtin.Int2048, 13

  // This should expand to the column number of the (, not to the column number
  // of __COLUMN__ in the default argument list of this function.
  // rdar://14315674
  magic_identifier_expansion()
  // CHECK: integer_literal $Builtin.Int2048, 29
}

func print_string() {
  // CHECK-LABEL: print_string
  var str = "\x08\x09\thello\r\n\0world\x1e\x7f"
  // CHECK: string_literal utf16 "\x08\t\thello\r\n\0world\x1E\x7F"
}



// Test that we can silgen superclass calls that go farther than the immediate
// superclass.
class Super1 {
  func funge() {}
}
class Super2 : Super1 {}
class Super3 : Super2 {
  override func funge() {
    super.funge()
  }
}

// <rdar://problem/16880240> SILGen crash assigning to _
func testDiscardLValue() {
  var a = 42
  _ = a
}

