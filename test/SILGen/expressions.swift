// RUN: %swift -parse-stdlib -emit-silgen %s | FileCheck %s
import swift


struct SillyString : BuiltinStringLiteralConvertible, StringLiteralConvertible {
  static func _convertFromBuiltinStringLiteral(value : Builtin.RawPointer,
                                               byteSize : Builtin.Int64,
                                               isASCII : Builtin.Int1) -> SillyString { }
  typealias StringLiteralType = SillyString
  static func convertFromStringLiteral(value : SillyString) -> SillyString { }
}

func literals() {
  var a = 1
  var b = 1.25
  var c = 'x'
  var d = "foo"
  var e:SillyString = "foo"
}
// CHECK: sil @_T11expressions8literalsFT_T_
// CHECK: integer_literal $Builtin.Int128, 1
// CHECK: float_literal $Builtin.FPIEEE64, 0x3FF4000000000000
// CHECK: integer_literal $Builtin.Int21, 120
// CHECK: string_literal $(Builtin.RawPointer, Builtin.Int64, Builtin.Int1), "foo"
// CHECK: string_literal $(Builtin.RawPointer, Builtin.Int64, Builtin.Int1), "foo"

func bar(x:Int) {}
func bar(x:Int, y:Int) {}

func call_one() {
  bar(42);
}

// CHECK: sil @_T11expressions8call_oneFT_T_
// CHECK: [[BAR:%[0-9]+]] = function_ref @_T11expressions3barFT1xSi_T_ : $[thin] (x : Int64) -> ()
// CHECK: [[FORTYTWO:%[0-9]+]] = integer_literal {{.*}} 42
// CHECK: [[FORTYTWO_CONVERTED:%[0-9]+]] = apply {{.*}}([[FORTYTWO]], {{.*}})
// CHECK: apply [[BAR]]([[FORTYTWO_CONVERTED]])

func call_two() {
  bar(42, 219)
}

// CHECK: sil @_T11expressions8call_twoFT_T_
// CHECK: [[BAR:%[0-9]+]] = function_ref @_T11expressions3barFT1xSi1ySi_T_ : $[thin] (x : Int64, y : Int64) -> ()
// CHECK: [[FORTYTWO:%[0-9]+]] = integer_literal {{.*}} 42
// CHECK: [[FORTYTWO_CONVERTED:%[0-9]+]] = apply {{.*}}([[FORTYTWO]], {{.*}})
// CHECK: [[TWONINETEEN:%[0-9]+]] = integer_literal {{.*}} 219
// CHECK: [[TWONINETEEN_CONVERTED:%[0-9]+]] = apply {{.*}}([[TWONINETEEN]], {{.*}})
// CHECK: apply [[BAR]]([[FORTYTWO_CONVERTED]], [[TWONINETEEN_CONVERTED]])

func tuples() {
  bar((4, 5).1)

  var T1 : (a : Int16, b : Int) = (b : 42, a : 777)

  // varargs.
  var va1 : (Int...) = ()
  var va2 : (Int, Int...) = (1,2,3)
}

// CHECK: sil @_T11expressions6tuplesFT_T_


class C {
  var chi:Int
  constructor() {
    chi = 219
  }
  constructor(x:Int) {
    chi = x
  }
}

// CHECK: sil @_T11expressions7classesFT_T_
func classes() {
  // CHECK: function_ref @_TC11expressions1CCfMS0_FT_S0_ : $[thin] ((), C.metatype) -> C
  var a = C()
  // CHECK: function_ref @_TC11expressions1CCfMS0_FT1xSi_S0_ : $[thin] ((x : Int64), C.metatype) -> C
  var b = C(0)
}

struct S {
  var x:Int
  constructor() {
    x = 219
  }
  constructor(x : Int) {
    this.x = x
  }
}

// CHECK: sil @_T11expressions7structsFT_T_
func structs() {
  // CHECK: function_ref @_TV11expressions1SCfMS0_FT_S0_ : $[thin] ((), S.metatype) -> S
  var a = S()
  // CHECK: function_ref @_TV11expressions1SCfMS0_FT1xSi_S0_ : $[thin] ((x : Int64), S.metatype) -> S
  var b = S(0)
}


func byrefcallee(x : [byref] Int) {}
func address_of_expr() {
  var x : Int = 4
  byrefcallee(&x)
}



func identity<T>(x : T) -> T {}

// CHECK: sil @_T11expressions15specialize_exprFT_Si
func specialize_expr() -> Int {
  // CHECK: specialize {{.*}}, $[thin] (x : Int64) -> Int64, T = Int
   return identity(17)
}


// CHECK: sil @_T11expressions15scalar_to_tupleFT_T_
func scalar_to_tuple() {
  var a : (x : Int) = 42
  var b : (Int...) = 42
  var c : (Int, Int...) = 42
}

// CHECK: sil @_T11expressions11array_allocFT1nSi_T_
func array_alloc(n : Int) {
  var a : Int[] = new Int[n]
}

struct SomeStruct {
  func a() {}
}

// CHECK: sil @_T11expressions5callsFT_T_
// CHECK: [[METHOD:%[0-9]+]] = function_ref @_TV11expressions10SomeStruct1afRS0_FT_T_ : $[cc(method), thin] ((), [byref] SomeStruct) -> ()
// CHECK: apply [[METHOD]]({{.*}})
func calls() {
  var a : SomeStruct
  a.a()
}

// CHECK: sil @_T11expressions11module_pathFT_Sb
func module_path() -> Bool {
  return swift.true
  // CHECK: [[TRUE_GET:%[0-9]+]] = function_ref @_TSs4trueSbg
  // CHECK: apply [[TRUE_GET]]()
}

// CHECK: sil @_T11expressions13module_path_2FT_Sb
func module_path_2() -> Bool {
  var slimy = swift
  return slimy.true
  // CHECK: [[TRUE_GET:%[0-9]+]] = function_ref @_TSs4trueSbg
  // CHECK: apply [[TRUE_GET]]()
}

func default_args(x:Int, y:Int = 219, z:Int = 20721) {}

// CHECK: sil @_T11expressions19call_default_args_1FT1xSi_T_
func call_default_args_1(x:Int) {
  default_args(x)
  // CHECK: [[XADDR:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T11expressions12default_argsFT1xSi1ySi1zSi_T_
  // CHECK: [[X:%[0-9]+]] = load [[XADDR]]
  // CHECK: [[YFUNC:%[0-9]+]] = function_ref @_T11expressions12default_argsFT1xSi1ySi1zSi_T_e0_
  // CHECK: [[Y:%[0-9]+]] = apply [[YFUNC]]()
  // CHECK: [[ZFUNC:%[0-9]+]] = function_ref @_T11expressions12default_argsFT1xSi1ySi1zSi_T_e1_
  // CHECK: [[Z:%[0-9]+]] = apply [[ZFUNC]]()
  // CHECK: apply [[FUNC]]([[X]], [[Y]], [[Z]])
}

// CHECK: sil @_T11expressions19call_default_args_2FT1xSi1zSi_T_
func call_default_args_2(x:Int, z:Int) {
  default_args(x, z:z)
  // CHECK: [[XADDR:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[ZADDR:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T11expressions12default_argsFT1xSi1ySi1zSi_T_
  // CHECK: [[X:%[0-9]+]] = load [[XADDR]]
  // CHECK: [[Z:%[0-9]+]] = load [[ZADDR]]
  // CHECK: [[DEFFN:%[0-9]+]] = function_ref @_T11expressions12default_argsFT1xSi1ySi1zSi_T_e0_
  // CHECK-NEXT: [[C219:%[0-9]+]] = apply [[DEFFN]]()
  // CHECK: apply [[FUNC]]([[X]], [[C219]], [[Z]])
}

struct Generic<T> {
  var mono_member:Int
  var typevar_member:T

  // CHECK: sil @_TV11expressions7Generic13type_variableU__fRGS0_Q__FT_MQ_
  func type_variable() -> T.metatype {
    return T
    // CHECK: [[METATYPE:%[0-9]+]] = metatype $T.metatype
    // CHECK: return [[METATYPE]]
  }

  // CHECK: sil @_TV11expressions7Generic19copy_typevar_memberU__fRGS0_Q__FT1xGS0_Q___T_
  func copy_typevar_member(x:Generic<T>) {
    typevar_member = x.typevar_member
  }

  // CHECK: sil @_TV11expressions7Generic12class_methodU__fMGS0_Q__FT_T_
  static func class_method() {}
}

// CHECK: sil @_T11expressions18generic_member_refU__FT1xGVS_7GenericQ___Si
func generic_member_ref<T>(x:Generic<T>) -> Int {
  // CHECK: bb0([[XADDR:%[0-9]+]] : $*Generic<T>):
  return x.mono_member
  // CHECK: [[MEMBER_ADDR:%[0-9]+]] = struct_element_addr {{.*}}, #mono_member
  // CHECK: load [[MEMBER_ADDR]]
}

// CHECK: sil @_T11expressions24bound_generic_member_refFT1xGVS_7GenericSc__Si
func bound_generic_member_ref(x:Generic<Char>) -> Int {
  // CHECK: bb0([[XADDR:%[0-9]+]] : $*Generic<Char>):
  return x.mono_member
  // CHECK: [[MEMBER_ADDR:%[0-9]+]] = struct_element_addr {{.*}}, #mono_member
  // CHECK: load [[MEMBER_ADDR]]
}

// CHECK: sil @_T11expressions6coerceFT1xVSs5Int32_Si
func coerce(x:Int32) -> Int64 {
  return 0 as Int64
}

class B {
}

class D : B {
}

// CHECK: sil @_T11expressions8downcastFT1xCS_1B_CS_1D
func downcast(x:B) -> D {
  return x as! D
  // CHECK: downcast unconditional %{{[0-9]+}} : {{.*}} to $D
}

// CHECK: sil @_T11expressions6upcastFT1xCS_1D_CS_1B
func upcast(x:D) -> B {
  return x
  // CHECK: upcast %{{[0-9]+}} : ${{.*}} to $B
}

// CHECK: sil @_T11expressions14generic_upcastU__FT1xQ__CS_1B
func generic_upcast<T:B>(x:T) -> B {
  return x
  // CHECK: archetype_ref_to_super %{{.*}} to $B
  // CHECK: return
}

// CHECK: sil @_T11expressions16generic_downcastU__FT1xQ_1yCS_1B_Q_
func generic_downcast<T:B>(x:T, y:B) -> T {
  return y as! T
  // CHECK: super_to_archetype_ref unconditional %{{[0-9]+}} : {{.*}} to $T
  // CHECK: return
}

// TODO: generic_downcast

// CHECK: sil @_T11expressions15metatype_upcastFT_MCS_1B
func metatype_upcast() -> B.metatype {
  return D
  // CHECK: metatype $D
  // CHECK-NEXT: upcast
}

// CHECK: sil @_T11expressions19interpolated_stringFT1xSi1ySS_SS
func interpolated_string(x:Int, y:String) -> String {
  return "The \(x) Million Dollar \(y)"
}

protocol Runcible {
  typealias U
  var free:Int
  var associated:U

  func free_method() -> Int
  func associated_method() -> U.metatype
  static func static_method()
}

protocol Bendable { }
protocol Wibbleable { }

// CHECK: sil @_T11expressions20archetype_member_refUS_8Runcible___FT1xQ__T_
func archetype_member_ref<T:Runcible>(x:T) {
  x.free_method()
  // CHECK: archetype_method
  // CHECK-NEXT: apply
  var u = x.associated_method()
  T.static_method()
}

// CHECK: sil @_T11expressions22existential_member_refFT1xPS_8Runcible__T_
func existential_member_ref(x:Runcible) {
  x.free_method()
  // CHECK: project_existential
  // CHECK-NEXT: protocol_method
  // CHECK-NEXT: apply
}

/*TODO archetype and existential properties and subscripts
func archetype_property_ref<T:Runcible>(x:T) -> (Int, T.U) {
  x.free = x.free_method()
  x.associated = x.associated_method()
  return (x.free, x.associated)
}

func existential_property_ref<T:Runcible>(x:T) -> Int {
  x.free = x.free_method()
  return x.free
}

also archetype/existential subscripts
*/

struct Spoon : Runcible {
  typealias U = Float
  var free:Int
  var associated:Float

  func free_method() -> Int {}
  func associated_method() -> Float.metatype {}
  static func static_method() {}
}

struct Hat<T> : Runcible {
  typealias U = T[]
  var free:Int
  var associated:U

  func free_method() -> Int {}

  // CHECK: sil @_TV11expressions3Hat17associated_methodU__fRGS0_Q__FT_MGSaQ__
  func associated_method() -> U.metatype {
    return U
    // CHECK: [[META:%[0-9]+]] = metatype $Slice<T>.metatype
    // CHECK: return [[META]]
  }

  static func static_method() {}
}

// CHECK: sil @_T11expressions7erasureFT1xVS_5Spoon_PS_8Runcible_
func erasure(x:Spoon) -> Runcible {
  return x
  // CHECK: init_existential
  // CHECK: return
}

// CHECK: sil @_T11expressions18erasure_from_protoFT1xPS_8BendableS_10Wibbleable__PS0__
func erasure_from_proto(x:protocol<Wibbleable, Bendable>) -> Bendable {
  return x
  // CHECK: upcast_existential
  // CHECK: return
}

// CHECK: sil @_T11expressions19declref_to_metatypeFT_MVS_5Spoon
func declref_to_metatype() -> Spoon.metatype {
  return Spoon
  // CHECK: metatype $Spoon.metatype
}

// CHECK: sil @_T11expressions27declref_to_generic_metatypeFT_MGVS_7GenericSc_
func declref_to_generic_metatype() -> Generic<Char>.metatype {
  // FIXME parsing of T<U> in expression context
  typealias GenericChar = Generic<Char>
  return GenericChar
  // CHECK: metatype $Generic<Char>.metatype
}

func int(x:Int) {}
func float(x:Float) {}

func tuple() -> (Int, Float) { return (1, 1.0) }

// CHECK: sil @_T11expressions13tuple_elementFT1xTSiSf__T_
func tuple_element(x:(Int, Float)) {
  // CHECK: [[XADDR:%.*]] = alloc_box $(Int64, Float32)

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

// CHECK: sil @_T11expressions10containersFT_TGSaSi_GCSs10DictionarySSSi__
func containers() -> (Int[], Dictionary<String, Int>) {
  return ([1, 2, 3], ["Ankeny": 1, "Burnside": 2, "Couch": 3])
}

// CHECK: sil @_T11expressions7if_exprFT1aSb1bSb1xSi1ySi1zSi_Si
func if_expr(a:Bool, b:Bool, x:Int, y:Int, z:Int) -> Int {
  // CHECK: bb0({{.*}}):
  // CHECK: [[A:%[0-9]+]] = alloc_box $Bool
  // CHECK: [[B:%[0-9]+]] = alloc_box $Bool
  // CHECK: [[X:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[Y:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[Z:%[0-9]+]] = alloc_box $Int64

  return a
    ? x
    : b
    ? y
    : z
  // CHECK:   [[ACOND:%[0-9]+]] = apply {{.*}}([[A]]#1)
  // CHECK:   condbranch [[ACOND]], [[IF_A:bb.*]], [[ELSE_A:bb.*]]
  // CHECK: [[IF_A]]:
  // CHECK:   [[XVAL:%[0-9]+]] = load [[X]]
  // CHECK:   br [[CONT_A:bb.*]]([[XVAL]] : $Int64)
  // CHECK: [[ELSE_A]]:
  // CHECK:   [[BCOND:%[0-9]+]] = apply {{.*}}([[B]]#1)
  // CHECK:   condbranch [[BCOND]], [[IF_B:bb.*]], [[ELSE_B:bb.*]]
  // CHECK: [[IF_B]]:
  // CHECK:   [[YVAL:%[0-9]+]] = load [[Y]]
  // CHECK:   br [[CONT_B:bb.*]]([[YVAL]] : $Int64)
  // CHECK: [[ELSE_B]]:
  // CHECK:   [[ZVAL:%[0-9]+]] = load [[Z]]
  // CHECK:   br [[CONT_B:bb.*]]([[ZVAL]] : $Int64)
  // CHECK: [[CONT_B]]([[B_RES:%[0-9]+]] : $Int64):
  // CHECK:   br [[CONT_A:bb.*]]([[B_RES]] : $Int64)
  // CHECK: [[CONT_A]]([[A_RES:%[0-9]+]] : $Int64):
  // CHECK:   return [[A_RES]]
}


// Test that magic identifiers expand properly.  We test __COLUMN__ here because
// it isn't affected as this testcase slides up and down the file over time.
func magic_identifier_expansion(a : Int = __COLUMN__) {
  // CHECK: sil @{{.*}}magic_identifier_expansion
  
  // This should expand to the column number of the first _.
  var tmp = __COLUMN__
  // CHECK: integer_literal $Builtin.Int128, 13

  // This should expand to the column number of the (, not to the column number
  // of __COLUMN__ in the default argument list of this function.
  // rdar://14315674
  magic_identifier_expansion()
  // CHECK: integer_literal $Builtin.Int128, 29
}

