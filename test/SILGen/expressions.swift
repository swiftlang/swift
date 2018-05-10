
// RUN: %empty-directory(%t)
// RUN: echo "public var x = Int()" | %target-swift-frontend -module-name FooBar -emit-module -o %t -
// RUN: %target-swift-emit-silgen -parse-stdlib -module-name expressions -enable-sil-ownership %s -I%t -disable-access-control | %FileCheck %s

import Swift
import FooBar

struct SillyString : _ExpressibleByBuiltinStringLiteral, ExpressibleByStringLiteral {
  init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {}

  init(unicodeScalarLiteral value: SillyString) { }

  init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1
  ) { 
  }

  init(extendedGraphemeClusterLiteral value: SillyString) { }

  init(
    _builtinStringLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1) { 
  }

  init(stringLiteral value: SillyString) { }
}

struct SillyUTF16String : _ExpressibleByBuiltinUTF16StringLiteral, ExpressibleByStringLiteral {
  init(_builtinUnicodeScalarLiteral value: Builtin.Int32) { }

  init(unicodeScalarLiteral value: SillyString) { }

  init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1
  ) { 
  }

  init(extendedGraphemeClusterLiteral value: SillyString) { }

  init(
    _builtinStringLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1
  ) { }

  init(
    _builtinUTF16StringLiteral start: Builtin.RawPointer,
    utf16CodeUnitCount: Builtin.Word
  ) { 
  }

  init(stringLiteral value: SillyUTF16String) { }
}

struct SillyConstUTF16String : _ExpressibleByBuiltinConstUTF16StringLiteral, ExpressibleByStringLiteral {
  init(_builtinUnicodeScalarLiteral value: Builtin.Int32) { }

  init(unicodeScalarLiteral value: SillyString) { }

  init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1
  ) {
  }

  init(extendedGraphemeClusterLiteral value: SillyString) { }

  init( _builtinConstStringLiteral start: Builtin.RawPointer) { }

  init( _builtinConstUTF16StringLiteral start: Builtin.RawPointer) { }

  init(stringLiteral value: SillyUTF16String) { }
}

func literals() {
  var a = 1
  var b = 1.25
  var d = "foö"
  var e:SillyString = "foo"

  var f:SillyConstUTF16String = "foobar"
  var non_ascii:SillyConstUTF16String = "foobarö"
}
// CHECK-LABEL: sil hidden @$S11expressions8literalsyyF
// CHECK: integer_literal $Builtin.Int2048, 1
// CHECK: float_literal $Builtin.FPIEEE{{64|80}}, {{0x3FF4000000000000|0x3FFFA000000000000000}}
// CHECK: string_literal utf16 "foö"
// CHECK: string_literal utf8 "foo"
// CHECK: [[CONST_STRING_LIT:%.*]] = const_string_literal utf8 "foobar"
// CHECK: [[METATYPE:%.*]] = metatype $@thin SillyConstUTF16String.Type
// CHECK: [[FUN:%.*]] = function_ref @$S11expressions21SillyConstUTF16StringV08_builtincE7LiteralACBp_tcfC : $@convention(method) (Builtin.RawPointer, @thin SillyConstUTF16String.Type) -> SillyConstUTF16String
// CHECK: apply [[FUN]]([[CONST_STRING_LIT]], [[METATYPE]]) : $@convention(method) (Builtin.RawPointer, @thin SillyConstUTF16String.Type) -> SillyConstUTF16String
// CHECK: [[CONST_UTF16STRING_LIT:%.*]] = const_string_literal utf16 "foobarö"
// CHECK: [[FUN:%.*]] = function_ref @$S11expressions21SillyConstUTF16StringV08_builtincdE7LiteralACBp_tcfC : $@convention(method) (Builtin.RawPointer, @thin SillyConstUTF16String.Type) -> SillyConstUTF16String
// CHECK: apply [[FUN]]([[CONST_UTF16STRING_LIT]], {{.*}}) : $@convention(method) (Builtin.RawPointer, @thin SillyConstUTF16String.Type) -> SillyConstUTF16String

func bar(_ x: Int) {}
func bar(_ x: Int, _ y: Int) {}

func call_one() {
  bar(42);
}

// CHECK-LABEL: sil hidden @$S11expressions8call_oneyyF
// CHECK: [[FORTYTWO:%[0-9]+]] = integer_literal {{.*}} 42
// CHECK: [[FORTYTWO_CONVERTED:%[0-9]+]] = apply {{.*}}([[FORTYTWO]], {{.*}})
// CHECK: [[BAR:%[0-9]+]] = function_ref @$S11expressions3bar{{[_0-9a-zA-Z]*}}F : $@convention(thin) (Int) -> ()
// CHECK: apply [[BAR]]([[FORTYTWO_CONVERTED]])

func call_two() {
  bar(42, 219)
}

// CHECK-LABEL: sil hidden @$S11expressions8call_twoyyF
// CHECK: [[FORTYTWO:%[0-9]+]] = integer_literal {{.*}} 42
// CHECK: [[FORTYTWO_CONVERTED:%[0-9]+]] = apply {{.*}}([[FORTYTWO]], {{.*}})
// CHECK: [[TWONINETEEN:%[0-9]+]] = integer_literal {{.*}} 219
// CHECK: [[TWONINETEEN_CONVERTED:%[0-9]+]] = apply {{.*}}([[TWONINETEEN]], {{.*}})
// CHECK: [[BAR:%[0-9]+]] = function_ref @$S11expressions3bar{{[_0-9a-zA-Z]*}}F : $@convention(thin) (Int, Int) -> ()
// CHECK: apply [[BAR]]([[FORTYTWO_CONVERTED]], [[TWONINETEEN_CONVERTED]])

func tuples() {
  bar((4, 5).1)

  var T1 : (a: Int16, b: Int) = (b : 42, a : 777)
}

// CHECK-LABEL: sil hidden @$S11expressions6tuplesyyF


class C {
  var chi:Int
  init() {
    chi = 219
  }
  init(x:Int) {
    chi = x
  }
}

// CHECK-LABEL: sil hidden @$S11expressions7classesyyF
func classes() {
  // CHECK: function_ref @$S11expressions1CC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thick C.Type) -> @owned C
  var a = C()
  // CHECK: function_ref @$S11expressions1CC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (Int, @thick C.Type) -> @owned C
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

// CHECK-LABEL: sil hidden @$S11expressions7structsyyF
func structs() {
  // CHECK: function_ref @$S11expressions1SV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thin S.Type) -> S
  var a = S()
  // CHECK: function_ref @$S11expressions1SV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (Int, @thin S.Type) -> S
  var b = S(x: 0)
}


func inoutcallee(_ x: inout Int) {}
func address_of_expr() {
  var x: Int = 4
  inoutcallee(&x)
}



func identity<T>(_ x: T) -> T {}

struct SomeStruct {
 mutating
  func a() {}
}

// CHECK-LABEL: sil hidden @$S11expressions5callsyyF
// CHECK: [[METHOD:%[0-9]+]] = function_ref @$S11expressions10SomeStructV1a{{[_0-9a-zA-Z]*}}F : $@convention(method) (@inout SomeStruct) -> ()
// CHECK: apply [[METHOD]]({{.*}})
func calls() {
  var a : SomeStruct
  a.a()
}

// CHECK-LABEL: sil hidden @$S11expressions11module_path{{[_0-9a-zA-Z]*}}F
func module_path() -> Int {
  return FooBar.x
  // CHECK: [[x_GET:%[0-9]+]] = function_ref @$S6FooBar1xSivau
  // CHECK-NEXT: apply [[x_GET]]()
}

func default_args(_ x: Int, y: Int = 219, z: Int = 20721) {}

// CHECK-LABEL: sil hidden @$S11expressions19call_default_args_1{{[_0-9a-zA-Z]*}}F
func call_default_args_1(_ x: Int) {
  default_args(x)
  // CHECK: [[YFUNC:%[0-9]+]] = function_ref @$S11expressions12default_args{{[_0-9a-zA-Z]*}}A0_
  // CHECK: [[Y:%[0-9]+]] = apply [[YFUNC]]()
  // CHECK: [[ZFUNC:%[0-9]+]] = function_ref @$S11expressions12default_args{{[_0-9a-zA-Z]*}}A1_
  // CHECK: [[Z:%[0-9]+]] = apply [[ZFUNC]]()
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @$S11expressions12default_args{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[FUNC]]({{.*}}, [[Y]], [[Z]])
}

// CHECK-LABEL: sil hidden @$S11expressions19call_default_args_2{{[_0-9a-zA-Z]*}}F
func call_default_args_2(_ x: Int, z: Int) {
  default_args(x, z:z)
  // CHECK: [[DEFFN:%[0-9]+]] = function_ref @$S11expressions12default_args{{[_0-9a-zA-Z]*}}A0_
  // CHECK-NEXT: [[C219:%[0-9]+]] = apply [[DEFFN]]()
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @$S11expressions12default_args{{[_0-9a-zA-Z]*}}F
  // CHECK-NEXT: apply [[FUNC]]({{.*}}, [[C219]], {{.*}})
}

struct Generic<T> {
  var mono_member:Int
  var typevar_member:T

  // CHECK-LABEL: sil hidden @$S11expressions7GenericV13type_variable{{[_0-9a-zA-Z]*}}F
  mutating
  func type_variable() -> T.Type {
    return T.self
    // CHECK: [[METATYPE:%[0-9]+]] = metatype $@thick T.Type
    // CHECK: return [[METATYPE]]
  }

  // CHECK-LABEL: sil hidden @$S11expressions7GenericV19copy_typevar_member{{[_0-9a-zA-Z]*}}F
  mutating
  func copy_typevar_member(_ x: Generic<T>) {
    typevar_member = x.typevar_member
  }

  // CHECK-LABEL: sil hidden @$S11expressions7GenericV12class_method{{[_0-9a-zA-Z]*}}FZ
  static func class_method() {}
}

// CHECK-LABEL: sil hidden @$S11expressions18generic_member_ref{{[_0-9a-zA-Z]*}}F
func generic_member_ref<T>(_ x: Generic<T>) -> Int {
  // CHECK: bb0([[XADDR:%[0-9]+]] : @trivial $*Generic<T>):
  return x.mono_member
  // CHECK: [[MEMBER_ADDR:%[0-9]+]] = struct_element_addr {{.*}}, #Generic.mono_member
  // CHECK: load [trivial] [[MEMBER_ADDR]]
}

// CHECK-LABEL: sil hidden @$S11expressions24bound_generic_member_ref{{[_0-9a-zA-Z]*}}F
func bound_generic_member_ref(_ x: Generic<UnicodeScalar>) -> Int {
  var x = x
  // CHECK: bb0([[XADDR:%[0-9]+]] : @trivial $Generic<Unicode.Scalar>):
  return x.mono_member
  // CHECK: [[MEMBER_ADDR:%[0-9]+]] = struct_element_addr {{.*}}, #Generic.mono_member
  // CHECK: load [trivial] [[MEMBER_ADDR]]
}

// CHECK-LABEL: sil hidden @$S11expressions6coerce{{[_0-9a-zA-Z]*}}F
func coerce(_ x: Int32) -> Int64 {
  return 0
}

class B {
}

class D : B {
}

// CHECK-LABEL: sil hidden @$S11expressions8downcast{{[_0-9a-zA-Z]*}}F
func downcast(_ x: B) -> D {
  return x as! D
  // CHECK: unconditional_checked_cast %{{[0-9]+}} : {{.*}} to $D
}

// CHECK-LABEL: sil hidden @$S11expressions6upcast{{[_0-9a-zA-Z]*}}F
func upcast(_ x: D) -> B {
  return x
  // CHECK: upcast %{{[0-9]+}} : ${{.*}} to $B
}

// CHECK-LABEL: sil hidden @$S11expressions14generic_upcast{{[_0-9a-zA-Z]*}}F
func generic_upcast<T : B>(_ x: T) -> B {
  return x
  // CHECK: upcast %{{.*}} to $B
  // CHECK: return
}

// CHECK-LABEL: sil hidden @$S11expressions16generic_downcast{{[_0-9a-zA-Z]*}}F
func generic_downcast<T : B>(_ x: T, y: B) -> T {
  return y as! T
  // CHECK: unconditional_checked_cast %{{[0-9]+}} : {{.*}} to $T
  // CHECK: return
}

// TODO: generic_downcast

// CHECK-LABEL: sil hidden @$S11expressions15metatype_upcast{{[_0-9a-zA-Z]*}}F
func metatype_upcast() -> B.Type {
  return D.self
  // CHECK: metatype $@thick D
  // CHECK-NEXT: upcast
}

// CHECK-LABEL: sil hidden @$S11expressions19interpolated_string{{[_0-9a-zA-Z]*}}F
func interpolated_string(_ x: Int, y: String) -> String {
  return "The \(x) Million Dollar \(y)"
}

protocol Runcible {
  associatedtype U
  var free:Int { get }
  var associated:U { get }

  func free_method() -> Int
  mutating func associated_method() -> U.Type
  static func static_method()
}

protocol Mincible {
  var free:Int { get }
  func free_method() -> Int
  static func static_method()
}

protocol Bendable { }
protocol Wibbleable { }

// CHECK-LABEL: sil hidden @$S11expressions20archetype_member_ref{{[_0-9a-zA-Z]*}}F
func archetype_member_ref<T : Runcible>(_ x: T) {
  var x = x
  x.free_method()
  // CHECK:      [[READ:%.*]] = begin_access [read] [unknown] [[X:%.*]]
  // CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $T
  // CHECK-NEXT: copy_addr [[READ]] to [initialization] [[TEMP]]
  // CHECK-NEXT: end_access [[READ]]
  // CHECK-NEXT: witness_method $T, #Runcible.free_method!1
  // CHECK-NEXT: apply
  // CHECK-NEXT: destroy_addr [[TEMP]]
  var u = x.associated_method()
  // CHECK:      [[WRITE:%.*]] = begin_access [modify] [unknown]
  // CHECK-NEXT: witness_method $T, #Runcible.associated_method!1
  // CHECK-NEXT: apply
  T.static_method()
  // CHECK:      metatype $@thick T.Type
  // CHECK-NEXT: witness_method $T, #Runcible.static_method!1
  // CHECK-NEXT: apply
}

// CHECK-LABEL: sil hidden @$S11expressions22existential_member_ref{{[_0-9a-zA-Z]*}}F
func existential_member_ref(_ x: Mincible) {
  x.free_method()
  // CHECK: open_existential_addr
  // CHECK-NEXT: witness_method
  // CHECK-NEXT: apply
}

/*TODO archetype and existential properties and subscripts
func archetype_property_ref<T : Runcible>(_ x: T) -> (Int, T.U) {
  x.free = x.free_method()
  x.associated = x.associated_method()
  return (x.free, x.associated)
}

func existential_property_ref<T : Runcible>(_ x: T) -> Int {
  x.free = x.free_method()
  return x.free
}

also archetype/existential subscripts
*/

struct Spoon : Runcible, Mincible {
  typealias U = Float
  var free: Int { return 4 }
  var associated: Float { return 12 }

  func free_method() -> Int {}
  func associated_method() -> Float.Type {}
  static func static_method() {}
}

struct Hat<T> : Runcible {
  typealias U = [T]
  var free: Int { return 1 }
  var associated: U { get {} }

  func free_method() -> Int {}

  // CHECK-LABEL: sil hidden @$S11expressions3HatV17associated_method{{[_0-9a-zA-Z]*}}F
  mutating
  func associated_method() -> U.Type {
    return U.self
    // CHECK: [[META:%[0-9]+]] = metatype $@thin Array<T>.Type
    // CHECK: return [[META]]
  }

  static func static_method() {}
}

// CHECK-LABEL: sil hidden @$S11expressions7erasure{{[_0-9a-zA-Z]*}}F
func erasure(_ x: Spoon) -> Mincible {
  return x
  // CHECK: init_existential_addr
  // CHECK: return
}

// CHECK-LABEL: sil hidden @$S11expressions19declref_to_metatypeAA5SpoonVmyF
func declref_to_metatype() -> Spoon.Type {
  return Spoon.self
  // CHECK: metatype $@thin Spoon.Type
}

// CHECK-LABEL: sil hidden @$S11expressions27declref_to_generic_metatype{{[_0-9a-zA-Z]*}}F
func declref_to_generic_metatype() -> Generic<UnicodeScalar>.Type {
  // FIXME parsing of T<U> in expression context
  typealias GenericChar = Generic<UnicodeScalar>
  return GenericChar.self
  // CHECK: metatype $@thin Generic<Unicode.Scalar>.Type
}

func int(_ x: Int) {}
func float(_ x: Float) {}

func tuple() -> (Int, Float) { return (1, 1.0) }

// CHECK-LABEL: sil hidden @$S11expressions13tuple_element{{[_0-9a-zA-Z]*}}F
func tuple_element(_ x: (Int, Float)) {
  var x = x
  // CHECK: [[XADDR:%.*]] = alloc_box ${ var (Int, Float) }
  // CHECK: [[PB:%.*]] = project_box [[XADDR]]

  int(x.0)
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PB]]
  // CHECK: tuple_element_addr [[READ]] : {{.*}}, 0
  // CHECK: apply

  float(x.1)
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PB]]
  // CHECK: tuple_element_addr [[READ]] : {{.*}}, 1
  // CHECK: apply

  int(tuple().0)
  // CHECK: [[ZERO:%.*]] = tuple_extract {{%.*}} : {{.*}}, 0
  // CHECK: apply {{.*}}([[ZERO]])

  float(tuple().1)
  // CHECK: [[ONE:%.*]] = tuple_extract {{%.*}} : {{.*}}, 1
  // CHECK: apply {{.*}}([[ONE]])
}

// CHECK-LABEL: sil hidden @$S11expressions10containers{{[_0-9a-zA-Z]*}}F
func containers() -> ([Int], Dictionary<String, Int>) {
  return ([1, 2, 3], ["Ankeny": 1, "Burnside": 2, "Couch": 3])
}

// CHECK-LABEL: sil hidden @$S11expressions7if_expr{{[_0-9a-zA-Z]*}}F
func if_expr(_ a: Bool, b: Bool, x: Int, y: Int, z: Int) -> Int {
  var a = a
  var b = b
  var x = x
  var y = y
  var z = z
  // CHECK: bb0({{.*}}):
  // CHECK: [[AB:%[0-9]+]] = alloc_box ${ var Bool }
  // CHECK: [[PBA:%.*]] = project_box [[AB]]
  // CHECK: [[BB:%[0-9]+]] = alloc_box ${ var Bool }
  // CHECK: [[PBB:%.*]] = project_box [[BB]]
  // CHECK: [[XB:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[PBX:%.*]] = project_box [[XB]]
  // CHECK: [[YB:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[PBY:%.*]] = project_box [[YB]]
  // CHECK: [[ZB:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[PBZ:%.*]] = project_box [[ZB]]

  return a
    ? x
    : b
    ? y
    : z
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBA]]
  // CHECK:   [[A:%[0-9]+]] = load [trivial] [[READ]]
  // CHECK:   [[ACOND:%[0-9]+]] = apply {{.*}}([[A]])
  // CHECK:   cond_br [[ACOND]], [[IF_A:bb[0-9]+]], [[ELSE_A:bb[0-9]+]]
  // CHECK: [[IF_A]]:
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBX]]
  // CHECK:   [[XVAL:%[0-9]+]] = load [trivial] [[READ]]
  // CHECK:   br [[CONT_A:bb[0-9]+]]([[XVAL]] : $Int)
  // CHECK: [[ELSE_A]]:
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBB]]
  // CHECK:   [[B:%[0-9]+]] = load [trivial] [[READ]]
  // CHECK:   [[BCOND:%[0-9]+]] = apply {{.*}}([[B]])
  // CHECK:   cond_br [[BCOND]], [[IF_B:bb[0-9]+]], [[ELSE_B:bb[0-9]+]]
  // CHECK: [[IF_B]]:
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBY]]
  // CHECK:   [[YVAL:%[0-9]+]] = load [trivial] [[READ]]
  // CHECK:   br [[CONT_B:bb[0-9]+]]([[YVAL]] : $Int)
  // CHECK: [[ELSE_B]]:
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBZ]]
  // CHECK:   [[ZVAL:%[0-9]+]] = load [trivial] [[READ]]
  // CHECK:   br [[CONT_B:bb[0-9]+]]([[ZVAL]] : $Int)
  // CHECK: [[CONT_B]]([[B_RES:%[0-9]+]] : @trivial $Int):
  // CHECK:   br [[CONT_A:bb[0-9]+]]([[B_RES]] : $Int)
  // CHECK: [[CONT_A]]([[A_RES:%[0-9]+]] : @trivial $Int):
  // CHECK:   return [[A_RES]]
}


// Test that magic identifiers expand properly.  We test #column here because
// it isn't affected as this testcase slides up and down the file over time.
func magic_identifier_expansion(_ a: Int = #column) {
  // CHECK-LABEL: sil hidden @{{.*}}magic_identifier_expansion
  
  // This should expand to the column number of the first _.
  var tmp = #column
  // CHECK: integer_literal $Builtin.Int2048, 13

  // This should expand to the column number of the (, not to the column number
  // of #column in the default argument list of this function.
  // rdar://14315674
  magic_identifier_expansion()
  // CHECK: integer_literal $Builtin.Int2048, 29
}

func print_string() {
  // CHECK-LABEL: print_string
  var str = "\u{08}\u{09}\thello\r\n\0wörld\u{1e}\u{7f}"
  // CHECK: string_literal utf16 "\u{08}\t\thello\r\n\0wörld\u{1E}\u{7F}"
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


func dynamicTypePlusZero(_ a: Super1) -> Super1.Type {
  return type(of: a)
}
// CHECK-LABEL: dynamicTypePlusZero
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Super1):
// CHECK-NOT: copy_value
// CHECK: value_metatype  $@thick Super1.Type, [[ARG]] : $Super1

struct NonTrivialStruct {
  var c : Super1
  var x: NonTrivialStruct? {
    get { return nil }
    set {}
  }
}

func dontEmitIgnoredLoadExpr(_ a: NonTrivialStruct) -> NonTrivialStruct.Type {
  return type(of: a)
}
// CHECK-LABEL: dontEmitIgnoredLoadExpr
// CHECK: bb0(%0 : @guaranteed $NonTrivialStruct):
// CHECK-NEXT: debug_value
// CHECK-NEXT: [[RESULT:%.*]] = metatype $@thin NonTrivialStruct.Type
// CHECK-NEXT: return [[RESULT]] : $@thin NonTrivialStruct.Type

// Test that we evaluate the force unwrap to get its side effects (a potential trap),
// but don't actually need to perform the load of its value.
func dontLoadIgnoredLValueForceUnwrap(_ a: inout NonTrivialStruct?) -> NonTrivialStruct.Type {
  return type(of: a!)
}
// CHECK-LABEL: dontLoadIgnoredLValueForceUnwrap
// CHECK: bb0(%0 : @trivial $*Optional<NonTrivialStruct>):
// CHECK-NEXT: debug_value_addr %0
// CHECK-NEXT: [[READ:%[0-9]+]] = begin_access [read] [unknown] %0
// CHECK-NEXT: switch_enum_addr [[READ]] : $*Optional<NonTrivialStruct>, case #Optional.some!enumelt.1: bb2, case #Optional.none!enumelt: bb1
// CHECK: bb1:
// CHECK: unreachable
// CHECK: bb2:
// CHECK-NEXT: unchecked_take_enum_data_addr [[READ]] : $*Optional<NonTrivialStruct>, #Optional.some!enumelt.1
// CHECK-NEXT: end_access [[READ]]
// CHECK-NEXT: [[METATYPE:%[0-9]+]] = metatype $@thin NonTrivialStruct.Type
// CHECK-NEXT: return [[METATYPE]]

func dontLoadIgnoredLValueDoubleForceUnwrap(_ a: inout NonTrivialStruct??) -> NonTrivialStruct.Type {
  return type(of: a!!)
}
// CHECK-LABEL: dontLoadIgnoredLValueDoubleForceUnwrap
// CHECK: bb0(%0 : @trivial $*Optional<Optional<NonTrivialStruct>>):
// CHECK-NEXT: debug_value_addr %0
// CHECK-NEXT: [[READ:%[0-9]+]] = begin_access [read] [unknown] %0
// CHECK-NEXT: switch_enum_addr [[READ]] : $*Optional<Optional<NonTrivialStruct>>, case #Optional.some!enumelt.1: bb2, case #Optional.none!enumelt: bb1
// CHECK: bb1:
// CHECK: unreachable
// CHECK: bb2:
// CHECK-NEXT: [[UNWRAPPED:%[0-9]+]] = unchecked_take_enum_data_addr [[READ]] : $*Optional<Optional<NonTrivialStruct>>, #Optional.some!enumelt.1
// CHECK-NEXT: switch_enum_addr [[UNWRAPPED]] : $*Optional<NonTrivialStruct>, case #Optional.some!enumelt.1: bb4, case #Optional.none!enumelt: bb3
// CHECK: bb3:
// CHECK: unreachable
// CHECK: bb4:
// CHECK-NEXT: unchecked_take_enum_data_addr [[UNWRAPPED]] : $*Optional<NonTrivialStruct>, #Optional.some!enumelt.1
// CHECK-NEXT: end_access [[READ]]
// CHECK-NEXT: [[METATYPE:%[0-9]+]] = metatype $@thin NonTrivialStruct.Type
// CHECK-NEXT: return [[METATYPE]]

func loadIgnoredLValueForceUnwrap(_ a: inout NonTrivialStruct) -> NonTrivialStruct.Type {
  return type(of: a.x!)
}
// CHECK-LABEL: loadIgnoredLValueForceUnwrap
// CHECK: bb0(%0 : @trivial $*NonTrivialStruct):
// CHECK-NEXT: debug_value_addr %0
// CHECK-NEXT: [[READ:%[0-9]+]] = begin_access [read] [unknown] %0
// CHECK-NEXT: [[BORROW:%[0-9]+]] = load_borrow [[READ]]
// CHECK-NEXT: // function_ref NonTrivialStruct.x.getter
// CHECK-NEXT: [[GETTER:%[0-9]+]] = function_ref @$S{{[_0-9a-zA-Z]*}}vg : $@convention(method) (@guaranteed NonTrivialStruct) -> @owned Optional<NonTrivialStruct>
// CHECK-NEXT: [[X:%[0-9]+]] = apply [[GETTER]]([[BORROW]])
// CHECK-NEXT: end_borrow [[BORROW]] from [[READ]]
// CHECK-NEXT: end_access [[READ]]
// CHECK-NEXT: switch_enum [[X]] : $Optional<NonTrivialStruct>, case #Optional.some!enumelt.1: bb2, case #Optional.none!enumelt: bb1
// CHECK: bb1:
// CHECK: unreachable
// CHECK: bb2([[UNWRAPPED_X:%[0-9]+]] : @owned $NonTrivialStruct):
// CHECK-NEXT: destroy_value [[UNWRAPPED_X]]
// CHECK-NEXT: [[METATYPE:%[0-9]+]] = metatype $@thin NonTrivialStruct.Type
// CHECK-NEXT: return [[METATYPE]]

func loadIgnoredLValueThroughForceUnwrap(_ a: inout NonTrivialStruct?) -> NonTrivialStruct.Type {
  return type(of: a!.x!)
}
// CHECK-LABEL: loadIgnoredLValueThroughForceUnwrap
// CHECK: bb0(%0 : @trivial $*Optional<NonTrivialStruct>):
// CHECK-NEXT: debug_value_addr %0
// CHECK-NEXT: [[READ:%[0-9]+]] = begin_access [read] [unknown] %0
// CHECK-NEXT: switch_enum_addr [[READ]] : $*Optional<NonTrivialStruct>, case #Optional.some!enumelt.1: bb2, case #Optional.none!enumelt: bb1
// CHECK: bb1:
// CHECK: unreachable
// CHECK: bb2:
// CHECK-NEXT: [[UNWRAPPED:%[0-9]+]] = unchecked_take_enum_data_addr [[READ]] : $*Optional<NonTrivialStruct>, #Optional.some!enumelt.1
// CHECK-NEXT: [[BORROW:%[0-9]+]] = load_borrow [[UNWRAPPED]]
// CHECK-NEXT: // function_ref NonTrivialStruct.x.getter
// CHECK-NEXT: [[GETTER:%[0-9]+]] = function_ref @$S{{[_0-9a-zA-Z]*}}vg : $@convention(method) (@guaranteed NonTrivialStruct) -> @owned Optional<NonTrivialStruct>
// CHECK-NEXT: [[X:%[0-9]+]] = apply [[GETTER]]([[BORROW]])
// CHECK-NEXT: end_borrow [[BORROW]] from [[UNWRAPPED]]
// CHECK-NEXT: end_access [[READ]]
// CHECK-NEXT: switch_enum [[X]] : $Optional<NonTrivialStruct>, case #Optional.some!enumelt.1: bb4, case #Optional.none!enumelt: bb3
// CHECK: bb3:
// CHECK: unreachable
// CHECK: bb4([[UNWRAPPED_X:%[0-9]+]] : @owned $NonTrivialStruct):
// CHECK-NEXT: destroy_value [[UNWRAPPED_X]]
// CHECK-NEXT: [[METATYPE:%[0-9]+]] = metatype $@thin NonTrivialStruct.Type
// CHECK-NEXT: return [[METATYPE]]

func evaluateIgnoredKeyPathExpr(_ s: inout NonTrivialStruct, _ kp: WritableKeyPath<NonTrivialStruct, Int>) -> Int.Type {
  return type(of: s[keyPath: kp])
}
// CHECK-LABEL: evaluateIgnoredKeyPathExpr
// CHECK: bb0(%0 : @trivial $*NonTrivialStruct, %1 : @guaranteed $WritableKeyPath<NonTrivialStruct, Int>):
// CHECK-NEXT: debug_value_addr %0
// CHECK-NEXT: debug_value %1
// CHECK-NEXT: [[S_READ:%[0-9]+]] = begin_access [read] [unknown] %0
// CHECK-NEXT: [[S_TEMP:%[0-9]+]] = alloc_stack $NonTrivialStruct
// CHECK-NEXT: copy_addr [[S_READ]] to [initialization] [[S_TEMP]]
// CHECK-NEXT: [[KP_TEMP:%[0-9]+]] = copy_value %1
// CHECK-NEXT: [[KP:%[0-9]+]] = upcast [[KP_TEMP]]
// CHECK-NEXT: [[RESULT:%[0-9]+]] = alloc_stack $Int
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[PROJECT_FN:%[0-9]+]] = function_ref @$Ss23_projectKeyPathReadOnly{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT: [[KP_BORROW:%.*]] = begin_borrow [[KP]]
// CHECK-NEXT: apply [[PROJECT_FN]]<NonTrivialStruct, Int>([[RESULT]], [[S_TEMP]], [[KP_BORROW]])
// CHECK-NEXT: end_access [[S_READ]]
// CHECK-NEXT: end_borrow [[KP_BORROW]]
// CHECK-NEXT: dealloc_stack [[RESULT]]
// CHECK-NEXT: destroy_value [[KP]]
// CHECK-NEXT: destroy_addr [[S_TEMP]]
// CHECK-NEXT: dealloc_stack [[S_TEMP]]
// CHECK-NEXT: [[METATYPE:%[0-9]+]] = metatype $@thin Int.Type
// CHECK-NOT: destroy_value %1
// CHECK-NEXT: return [[METATYPE]]



// <rdar://problem/18851497> Swiftc fails to compile nested destructuring tuple binding
// CHECK-LABEL: sil hidden @$S11expressions21implodeRecursiveTupleyySi_Sit_SitSgF
// CHECK: bb0(%0 : @trivial $Optional<((Int, Int), Int)>):
func implodeRecursiveTuple(_ expr: ((Int, Int), Int)?) {

  // CHECK: bb2([[WHOLE:%.*]] : @trivial $((Int, Int), Int)):
  // CHECK-NEXT: [[X:%[0-9]+]] = tuple_extract [[WHOLE]] : $((Int, Int), Int), 0
  // CHECK-NEXT: [[X0:%[0-9]+]] = tuple_extract [[X]] : $(Int, Int), 0
  // CHECK-NEXT: [[X1:%[0-9]+]] = tuple_extract [[X]] : $(Int, Int), 1
  // CHECK-NEXT: [[Y:%[0-9]+]] = tuple_extract [[WHOLE]] : $((Int, Int), Int), 1
  // CHECK-NEXT: [[X:%[0-9]+]] = tuple ([[X0]] : $Int, [[X1]] : $Int)
  // CHECK-NEXT: debug_value [[X]] : $(Int, Int), let, name "x"
  // CHECK-NEXT: debug_value [[Y]] : $Int, let, name "y"

  let (x, y) = expr!
}

func test20087517() {
  class Color {
    static func greenColor() -> Color { return Color() }
  }
  let x: (Color?, Int) = (.greenColor(), 1)
}

func test20596042() {
  enum E {
    case thing1
    case thing2
  }

  func f() -> (E?, Int)? {
    return (.thing1, 1)
  }
}

func test21886435() {
  () = ()
}

