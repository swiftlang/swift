// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

func takesOptionalFunction(_: (() -> ())?) {}

struct CustomNull : ExpressibleByNilLiteral {
  init(nilLiteral: ()) {}
}

func takesANull(_: CustomNull) {}

// CHECK-LABEL: sil hidden [ossa] @$s8literals4testyyF : $@convention(thin) () -> ()
func test() {
  // CHECK: [[NIL:%.*]] = enum $Optional<@callee_guaranteed () -> ()>, #Optional.none!enumelt
  // CHECK: [[FN:%.*]] = function_ref @$s8literals21takesOptionalFunctionyyyycSgF
  // CHECK: apply [[FN]]([[NIL]])
  _ = takesOptionalFunction(nil)

  // CHECK: [[METATYPE:%.*]] = metatype $@thin CustomNull.Type
  // CHECK: [[NIL_FN:%.*]] = function_ref @$s8literals10CustomNullV10nilLiteralACyt_tcfC
  // CHECK: [[NIL:%.*]] = apply [[NIL_FN]]([[METATYPE]])
  // CHECK: [[FN:%.*]] = function_ref @$s8literals10takesANullyyAA10CustomNullVF
  // CHECK: apply [[FN]]([[NIL]])
  _ = takesANull(nil)
}

class CustomStringClass : ExpressibleByStringLiteral {
  required init(stringLiteral value: String) {}
  required init(extendedGraphemeClusterLiteral value: String) {}
  required init(unicodeScalarLiteral value: String) {}
}

class CustomStringSubclass : CustomStringClass {}

// CHECK-LABEL: sil hidden [ossa] @$s8literals27returnsCustomStringSubclassAA0cdE0CyF : $@convention(thin) () -> @owned CustomStringSubclass
// CHECK: [[METATYPE:%.*]] = metatype $@thick CustomStringSubclass.Type
// CHECK: [[UPCAST:%.*]] = upcast [[METATYPE]] : $@thick CustomStringSubclass.Type to $@thick CustomStringClass.Type
// CHECK: [[CTOR:%.*]] = class_method [[UPCAST]] : $@thick CustomStringClass.Type, #CustomStringClass.init!allocator : (CustomStringClass.Type) -> (String) -> CustomStringClass, $@convention(method) (@owned String, @thick CustomStringClass.Type) -> @owned CustomStringClass
// CHECK: [[RESULT:%.*]] = apply [[CTOR]]({{%.*}}, [[UPCAST]])
// CHECK: [[DOWNCAST:%.*]] = unchecked_ref_cast [[RESULT]] : $CustomStringClass to $CustomStringSubclass
// CHECK: return [[DOWNCAST]]
func returnsCustomStringSubclass() -> CustomStringSubclass {
  return "hello world"
}

class TakesArrayLiteral<Element> : ExpressibleByArrayLiteral {
  required init(arrayLiteral elements: Element...) {}
}

// CHECK-LABEL: sil hidden [ossa] @$s8literals18returnsCustomArrayAA05TakesD7LiteralCySiGyF : $@convention(thin) () -> @owned TakesArrayLiteral<Int>
// CHECK: [[TMP:%.*]] = apply %2(%0, %1) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK: [[ARRAY_LENGTH:%.*]] = integer_literal $Builtin.Word, 2
// CHECK: [[ALLOCATE_VARARGS:%.*]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
// CHECK: [[ARR_TMP:%.*]] = apply [[ALLOCATE_VARARGS]]<Int>([[ARRAY_LENGTH]])
// CHECK: ([[ARR:%.*]], [[ADDRESS:%.*]]) = destructure_tuple [[ARR_TMP]]
// CHECK: [[MDI:%.*]] = mark_dependence [[ADDRESS]]
// CHECK: [[POINTER:%.*]] = pointer_to_address [[MDI]]
// CHECK: store [[TMP:%.*]] to [trivial] [[POINTER]]
// CHECK: [[IDX1:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[POINTER1:%.*]] = index_addr [[POINTER]] : $*Int, [[IDX1]] : $Builtin.Word
// CHECK: store [[TMP:%.*]] to [trivial] [[POINTER1]]
// CHECK: [[FIN_FN:%.*]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
// CHECK: [[FIN_ARR:%.*]] = apply [[FIN_FN]]<Int>([[ARR]])
// CHECK: [[METATYPE:%.*]] = metatype $@thick TakesArrayLiteral<Int>.Type
// CHECK: [[CTOR:%.*]] = class_method [[METATYPE]] : $@thick TakesArrayLiteral<Int>.Type, #TakesArrayLiteral.init!allocator : <Element> (TakesArrayLiteral<Element>.Type) -> (Element...) -> TakesArrayLiteral<Element>, $@convention(method)
// CHECK: [[RESULT:%.*]] = apply [[CTOR]]<Int>([[FIN_ARR]], [[METATYPE]])
// CHECK: return [[RESULT]]
func returnsCustomArray() -> TakesArrayLiteral<Int> {
  // Use temporary to simplify generated_sil
  let tmp = 77
  return [tmp, tmp]
}

class Klass {}

// CHECK-LABEL: sil hidden [ossa] @$s8literals24returnsClassElementArrayAA05TakesE7LiteralCyAA5KlassCGyF : $@convention(thin) () -> @owned TakesArrayLiteral<Klass>
// CHECK: [[ARRAY_LENGTH:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[ALLOCATE_VARARGS:%.*]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
// CHECK: [[ARR_TMP:%.*]] = apply [[ALLOCATE_VARARGS]]<Klass>([[ARRAY_LENGTH]])
// CHECK: ([[ARR:%.*]], [[ADDRESS:%.*]]) = destructure_tuple [[ARR_TMP]]
// CHECK: [[MDI:%.*]] = mark_dependence [[ADDRESS]]
// CHECK: [[POINTER:%.*]] = pointer_to_address [[MDI]]
// CHECK: [[KLASS_METATYPE:%.*]] = metatype $@thick Klass.Type
// CHECK: [[CTOR:%.*]] = function_ref @$s8literals5KlassCACycfC : $@convention(method) (@thick Klass.Type) -> @owned Klass
// CHECK: [[TMP:%.*]] = apply [[CTOR]]([[KLASS_METATYPE]]) : $@convention(method) (@thick Klass.Type) -> @owned Klass
// CHECK: store [[TMP]] to [init] [[POINTER]]
// CHECK: [[FIN_FN:%.*]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
// CHECK: [[FIN_ARR:%.*]] = apply [[FIN_FN]]<Klass>([[ARR]])
// CHECK: [[METATYPE:%.*]] = metatype $@thick TakesArrayLiteral<Klass>.Type
// CHECK: [[CTOR:%.*]] = class_method [[METATYPE]] : $@thick TakesArrayLiteral<Klass>.Type, #TakesArrayLiteral.init!allocator : <Element> (TakesArrayLiteral<Element>.Type) -> (Element...) -> TakesArrayLiteral<Element>, $@convention(method)
// CHECK: [[RESULT:%.*]] = apply [[CTOR]]<Klass>([[FIN_ARR]], [[METATYPE]])
// CHECK: return [[RESULT]]
func returnsClassElementArray() -> TakesArrayLiteral<Klass> {
  return [Klass()]
}

struct Foo<T> {
  var t: T
}

// CHECK-LABEL: sil hidden [ossa] @$s8literals30returnsAddressOnlyElementArray1tAA05TakesF7LiteralCyAA3FooVyxGGAH_tlF : $@convention(thin) <T> (@in_guaranteed Foo<T>) -> @owned TakesArrayLiteral<Foo<T>>
// CHECK: [[ARRAY_LENGTH:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[ALLOCATE_VARARGS:%.*]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
// CHECK: [[ARR_TMP:%.*]] = apply [[ALLOCATE_VARARGS]]<Foo<T>>([[ARRAY_LENGTH]])
// CHECK: ([[ARR:%.*]], [[ADDRESS:%.*]]) = destructure_tuple [[ARR_TMP]]
// CHECK: [[MDI:%.*]] = mark_dependence [[ADDRESS]]
// CHECK: [[POINTER:%.*]] = pointer_to_address [[MDI]]
// CHECK: copy_addr %0 to [init] [[POINTER]] : $*Foo<T>
// CHECK: [[FIN_FN:%.*]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
// CHECK: [[FIN_ARR:%.*]] = apply [[FIN_FN]]<Foo<T>>([[ARR]])
// CHECK: [[METATYPE:%.*]] = metatype $@thick TakesArrayLiteral<Foo<T>>.Type
// CHECK: [[CTOR:%.*]] = class_method [[METATYPE]] : $@thick TakesArrayLiteral<Foo<T>>.Type, #TakesArrayLiteral.init!allocator : <Element> (TakesArrayLiteral<Element>.Type) -> (Element...) -> TakesArrayLiteral<Element>, $@convention(method)
// CHECK: [[RESULT:%.*]] = apply [[CTOR]]<Foo<T>>([[FIN_ARR]], [[METATYPE]])
// CHECK: return [[RESULT]]
func returnsAddressOnlyElementArray<T>(t: Foo<T>) -> TakesArrayLiteral<Foo<T>> {
  return [t]
}

// CHECK-LABEL: sil hidden [ossa] @$s8literals3FooV20returnsArrayFromSelfAA05TakesD7LiteralCyACyxGGyF : $@convention(method) <T> (@in_guaranteed Foo<T>) -> @owned TakesArrayLiteral<Foo<T>>
// CHECK: [[ARRAY_LENGTH:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[ALLOCATE_VARARGS:%.*]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
// CHECK: [[ARR_TMP:%.*]] = apply [[ALLOCATE_VARARGS]]<Foo<T>>([[ARRAY_LENGTH]])
// CHECK: ([[ARR:%.*]], [[ADDRESS:%.*]]) = destructure_tuple [[ARR_TMP]]
// CHECK: [[MDI:%.*]] = mark_dependence [[ADDRESS]]
// CHECK: [[POINTER:%.*]] = pointer_to_address [[MDI]]
// CHECK: copy_addr %0 to [init] [[POINTER]] : $*Foo<T>
// CHECK: [[FIN_FN:%.*]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
// CHECK: [[FIN_ARR:%.*]] = apply [[FIN_FN]]<Foo<T>>([[ARR]])
// CHECK: [[METATYPE:%.*]] = metatype $@thick TakesArrayLiteral<Foo<T>>.Type
// CHECK: [[CTOR:%.*]] = class_method [[METATYPE]] : $@thick TakesArrayLiteral<Foo<T>>.Type, #TakesArrayLiteral.init!allocator : <Element> (TakesArrayLiteral<Element>.Type) -> (Element...) -> TakesArrayLiteral<Element>, $@convention(method)
// CHECK: [[RESULT:%.*]] = apply [[CTOR]]<Foo<T>>([[FIN_ARR]], [[METATYPE]])
// CHECK: return [[RESULT]]
extension Foo {
  func returnsArrayFromSelf() -> TakesArrayLiteral<Foo<T>> {
    return [self]
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s8literals3FooV28returnsArrayFromMutatingSelfAA05TakesD7LiteralCyACyxGGyF : $@convention(method) <T> (@inout Foo<T>) -> @owned TakesArrayLiteral<Foo<T>>
// CHECK: [[ARRAY_LENGTH:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[ALLOCATE_VARARGS:%.*]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
// CHECK: [[ARR_TMP:%.*]] = apply [[ALLOCATE_VARARGS]]<Foo<T>>([[ARRAY_LENGTH]])
// CHECK: ([[ARR:%.*]], [[ADDRESS:%.*]]) = destructure_tuple [[ARR_TMP]]
// CHECK: [[MDI:%.*]] = mark_dependence [[ADDRESS]]
// CHECK: [[POINTER:%.*]] = pointer_to_address [[MDI]]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] %0 : $*Foo<T>
// CHECK: copy_addr [[ACCESS]] to [init] [[POINTER]] : $*Foo<T>
// CHECK: end_access [[ACCESS]] : $*Foo<T>
// CHECK: [[FIN_FN:%.*]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
// CHECK: [[FIN_ARR:%.*]] = apply [[FIN_FN]]<Foo<T>>([[ARR]])
// CHECK: [[METATYPE:%.*]] = metatype $@thick TakesArrayLiteral<Foo<T>>.Type
// CHECK: [[CTOR:%.*]] = class_method [[METATYPE]] : $@thick TakesArrayLiteral<Foo<T>>.Type, #TakesArrayLiteral.init!allocator : <Element> (TakesArrayLiteral<Element>.Type) -> (Element...) -> TakesArrayLiteral<Element>, $@convention(method)
// CHECK: [[RESULT:%.*]] = apply [[CTOR]]<Foo<T>>([[FIN_ARR]], [[METATYPE]])
// CHECK: return [[RESULT]]
extension Foo {
  mutating func returnsArrayFromMutatingSelf() -> TakesArrayLiteral<Foo<T>> {
    return [self]
  }
}

struct Foo2 {
  var k: Klass
}

// CHECK-LABEL: sil hidden [ossa] @$s8literals23returnsNonTrivialStructAA17TakesArrayLiteralCyAA4Foo2VGyF : $@convention(thin) () -> @owned TakesArrayLiteral<Foo2>
// CHECK: [[ARRAY_LENGTH:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[ALLOCATE_VARARGS:%.*]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
// CHECK: [[ARR_TMP:%.*]] = apply [[ALLOCATE_VARARGS]]<Foo2>([[ARRAY_LENGTH]])
// CHECK: ([[ARR:%.*]], [[ADDRESS:%.*]]) = destructure_tuple [[ARR_TMP]]
// CHECK: [[MDI:%.*]] = mark_dependence [[ADDRESS]]
// CHECK: [[POINTER:%.*]] = pointer_to_address [[MDI]]
// CHECK: [[METATYPE_FOO2:%.*]] = metatype $@thin Foo2.Type
// CHECK: [[METATYPE_KLASS:%.*]] = metatype $@thick Klass.Type
// CHECK: [[CTOR:%.*]] = function_ref @$s8literals5KlassCACycfC : $@convention(method) (@thick Klass.Type) -> @owned Klass
// CHECK: [[K:%.*]] = apply [[CTOR]]([[METATYPE_KLASS]]) : $@convention(method) (@thick Klass.Type) -> @owned Klass
// CHECK: [[CTOR:%.*]] = function_ref @$s8literals4Foo2V1kAcA5KlassC_tcfC : $@convention(method) (@owned Klass, @thin Foo2.Type) -> @owned Foo2
// CHECK: [[TMP:%.*]] = apply [[CTOR]]([[K]], [[METATYPE_FOO2]]) : $@convention(method) (@owned Klass, @thin Foo2.Type) -> @owned Foo2
// store [[TMP]] to [init] [[POINTER]] : $*Foo2
// CHECK: [[FIN_FN:%.*]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
// CHECK: [[FIN_ARR:%.*]] = apply [[FIN_FN]]<Foo2>([[ARR]])
// CHECK: [[METATYPE:%.*]] = metatype $@thick TakesArrayLiteral<Foo2>.Type
// CHECK: [[CTOR:%.*]] = class_method [[METATYPE]] : $@thick TakesArrayLiteral<Foo2>.Type, #TakesArrayLiteral.init!allocator : <Element> (TakesArrayLiteral<Element>.Type) -> (Element...) -> TakesArrayLiteral<Element>, $@convention(method)
// CHECK: [[RESULT:%.*]] = apply [[CTOR]]<Foo2>([[FIN_ARR]], [[METATYPE]])
// CHECK: return [[RESULT]]
func returnsNonTrivialStruct() -> TakesArrayLiteral<Foo2> {
  return [Foo2(k: Klass())]
}

// CHECK-LABEL: sil hidden [ossa] @$s8literals16NestedLValuePathV11wrapInArrayyyF : $@convention(method) (@inout NestedLValuePath) -> ()
// CHECK: [[METATYPE_NESTED:%.*]] = metatype $@thin NestedLValuePath.Type
// CHECK: [[ARRAY_LENGTH:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[ALLOCATE_VARARGS:%.*]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
// CHECK: [[ARR_TMP:%.*]] = apply [[ALLOCATE_VARARGS]]<NestedLValuePath>([[ARRAY_LENGTH]])
// CHECK: ([[ARR:%.*]], [[ADDRESS:%.*]]) = destructure_tuple [[ARR_TMP]]
// CHECK: [[MDI:%.*]] = mark_dependence [[ADDRESS]]
// CHECK: [[POINTER:%.*]] = pointer_to_address [[MDI]]

// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] %0 : $*NestedLValuePath
// CHECK: [[OTHER_FN:%.*]] = function_ref @$s8literals16NestedLValuePathV21otherMutatingFunctionACyF : $@convention(method) (@inout NestedLValuePath) -> @owned NestedLValuePath
// CHECK: [[TMP:%.*]] = apply [[OTHER_FN]]([[ACCESS]]) : $@convention(method) (@inout NestedLValuePath) -> @owned NestedLValuePath
// CHECK: end_access [[ACCESS]] : $*NestedLValuePath
// CHECK: store [[TMP]] to [init] [[POINTER]] : $*NestedLValuePath
// CHECK: [[FIN_FN:%.*]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
// CHECK: [[FIN_ARR:%.*]] = apply [[FIN_FN]]<NestedLValuePath>([[ARR]])
// CHECK: [[METATYPE:%.*]] = metatype $@thick TakesArrayLiteral<NestedLValuePath>.Type
// CHECK: [[CTOR:%.*]] = class_method [[METATYPE]] : $@thick TakesArrayLiteral<NestedLValuePath>.Type, #TakesArrayLiteral.init!allocator : <Element> (TakesArrayLiteral<Element>.Type) -> (Element...) -> TakesArrayLiteral<Element>, $@convention(method)
// CHECK: [[ARR_RESULT:%.*]] = apply [[CTOR]]<NestedLValuePath>([[FIN_ARR]], [[METATYPE]])
// CHECK: [[CTOR:%.*]] = function_ref @$s8literals16NestedLValuePathV3arrAcA17TakesArrayLiteralCyACG_tcfC : $@convention(method) (@owned TakesArrayLiteral<NestedLValuePath>, @thin NestedLValuePath.Type) -> @owned NestedLValuePath
// CHECK: [[RESULT:%.*]] = apply [[CTOR]]([[ARR_RESULT]], [[METATYPE_NESTED]]) : $@convention(method) (@owned TakesArrayLiteral<NestedLValuePath>, @thin NestedLValuePath.Type) -> @owned NestedLValuePath
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] %0 : $*NestedLValuePath
// CHECK: assign [[RESULT]] to [[ACCESS]] : $*NestedLValuePath
// CHECK: end_access [[ACCESS]] : $*NestedLValuePath
// CHECK: [[VOID:%.*]] = tuple ()
// CHECK: return [[VOID]] : $()
struct NestedLValuePath {
  var arr: TakesArrayLiteral<NestedLValuePath>

  mutating func wrapInArray() {
    self = NestedLValuePath(arr: [self.otherMutatingFunction()])
  }

  mutating func otherMutatingFunction() -> NestedLValuePath {
    return self
  }
}

protocol WrapsSelfInArray {}

// CHECK-LABEL: sil hidden [ossa] @$s8literals16WrapsSelfInArrayPAAE04wrapdE0AA05TakesE7LiteralCyAaB_pGyF : $@convention(method) <Self where Self : WrapsSelfInArray> (@inout Self) -> @owned TakesArrayLiteral<any WrapsSelfInArray>
// CHECK: [[ARRAY_LENGTH:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[ALLOCATE_VARARGS:%.*]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
// CHECK: [[ARR_TMP:%.*]] = apply [[ALLOCATE_VARARGS]]<any WrapsSelfInArray>([[ARRAY_LENGTH]])
// CHECK: ([[ARR:%.*]], [[ADDRESS:%.*]]) = destructure_tuple [[ARR_TMP]]
// CHECK: [[MDI:%.*]] = mark_dependence [[ADDRESS]]
// CHECK: [[POINTER:%.*]] = pointer_to_address [[MDI]]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] %0 : $*Self
// CHECK: [[EXISTENTIAL:%.*]] = init_existential_addr [[POINTER]] : $*any WrapsSelfInArray, $Self
// CHECK: copy_addr [[ACCESS]] to [init] [[EXISTENTIAL]] : $*Self
// CHECK: end_access [[ACCESS]] : $*Self
// CHECK: [[FIN_FN:%.*]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
// CHECK: [[FIN_ARR:%.*]] = apply [[FIN_FN]]<any WrapsSelfInArray>([[ARR]])
// CHECK: [[METATYPE:%.*]] = metatype $@thick TakesArrayLiteral<any WrapsSelfInArray>.Type
// CHECK: [[CTOR:%.*]] = class_method [[METATYPE]] : $@thick TakesArrayLiteral<any WrapsSelfInArray>.Type, #TakesArrayLiteral.init!allocator : <Element> (TakesArrayLiteral<Element>.Type) -> (Element...) -> TakesArrayLiteral<Element>, $@convention(method)
// CHECK: [[RESULT:%.*]] = apply [[CTOR]]<any WrapsSelfInArray>([[FIN_ARR]], [[METATYPE]])
// CHECK: return [[RESULT]]
extension WrapsSelfInArray {
  mutating func wrapInArray() -> TakesArrayLiteral<WrapsSelfInArray> {
    return [self]
  }
}

protocol FooProtocol {
  init()
}

func makeThrowing<T : FooProtocol>() throws -> T { return T() }
func makeBasic<T : FooProtocol>() -> T { return T() }

// CHECK-LABEL: sil hidden [ossa] @$s8literals15throwingElementSayxGyKAA11FooProtocolRzlF : $@convention(thin) <T where T : FooProtocol> () -> (@owned Array<T>, @error any Error)
// CHECK: [[ARRAY_LENGTH:%.*]] = integer_literal $Builtin.Word, 2
// CHECK: [[ALLOCATE_VARARGS:%.*]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
// CHECK: [[ARR_TMP:%.*]] = apply [[ALLOCATE_VARARGS]]<T>([[ARRAY_LENGTH]])
// CHECK: ([[ARR:%.*]], [[ADDRESS:%.*]]) = destructure_tuple [[ARR_TMP]]
// CHECK: [[MDI:%.*]] = mark_dependence [[ADDRESS]]
// CHECK: [[POINTER:%.*]] = pointer_to_address [[MDI]]
// CHECK: [[FN:%.*]] = function_ref @$s8literals9makeBasicxyAA11FooProtocolRzlF : $@convention(thin)
// CHECK: [[TMP:%.*]] = apply [[FN]]<T>([[POINTER]])
// CHECK: [[IDX:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[POINTER1:%.*]] = index_addr [[POINTER]] : $*T, [[IDX]] : $Builtin.Word
// CHECK: [[FN:%.*]] = function_ref @$s8literals12makeThrowingxyKAA11FooProtocolRzlF : $@convention(thin)
// CHECK: try_apply [[FN]]<T>([[POINTER1]]) : {{.*}} normal bb1, error bb2

// CHECK: bb1([[TMP:%.*]] : $()):
// CHECK: [[FIN_FN:%.*]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
// CHECK: [[FIN_ARR:%.*]] = apply [[FIN_FN]]<T>([[ARR]])
// CHECK: return [[FIN_ARR]]

// CHECK: bb2([[ERR:%.*]] : @owned $any Error):
// CHECK: destroy_addr [[POINTER]] : $*T
// CHECK: [[DEALLOC:%.*]] = function_ref @$ss29_deallocateUninitializedArrayyySayxGnlF
// CHECK: [[TMP:%.*]] = apply [[DEALLOC]]<T>([[ARR]])
// CHECK: throw [[ERR]] : $any Error
func throwingElement<T : FooProtocol>() throws -> [T] {
  return try [makeBasic(), makeThrowing()]
}

class TakesDictionaryLiteral<Key, Value> : ExpressibleByDictionaryLiteral {
  required init(dictionaryLiteral elements: (Key, Value)...) {}
}

// CHECK-LABEL: sil hidden [ossa] @$s8literals23returnsCustomDictionaryAA05TakesD7LiteralCyS2iGyF : $@convention(thin) () -> @owned TakesDictionaryLiteral<Int, Int> {
// CHECK: [[TMP_VAL:%.*]] = apply %2(%0, %1) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK: [[TMP:%.*]] = move_value [var_decl] [[TMP_VAL]] : $Int
// CHECK: [[ARRAY_LENGTH:%.*]] = integer_literal $Builtin.Word, 2
// CHECK: // function_ref _allocateUninitializedArray<A>(_:)
// CHECK: [[ALLOCATE_VARARGS:%.*]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: [[ARR_TMP:%.*]] = apply [[ALLOCATE_VARARGS]]<(Int, Int)>([[ARRAY_LENGTH]])
// CHECK: ([[ARR:%.*]], [[ADDRESS:%.*]]) = destructure_tuple [[ARR_TMP]]
// CHECK: [[MDI:%.*]] = mark_dependence [[ADDRESS]]
// CHECK: [[TUPLE_ADDR:%.*]] = pointer_to_address [[MDI]] : $Builtin.RawPointer to [strict] $*(Int, Int)
// CHECK: [[KEY_ADDR:%.*]] = tuple_element_addr [[TUPLE_ADDR]] : $*(Int, Int), 0
// CHECK: [[VALUE_ADDR:%.*]] = tuple_element_addr [[TUPLE_ADDR]] : $*(Int, Int), 1
// CHECK: store [[TMP]] to [trivial] [[KEY_ADDR]] : $*Int
// CHECK: store [[TMP]] to [trivial] [[VALUE_ADDR]] : $*Int
// CHECK: [[IDX1:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[TUPLE_ADDR1:%.*]] = index_addr [[TUPLE_ADDR]] : $*(Int, Int), [[IDX1]] : $Builtin.Word
// CHECK: [[KEY_ADDR:%.*]] = tuple_element_addr [[TUPLE_ADDR1]] : $*(Int, Int), 0
// CHECK: [[VALUE_ADDR:%.*]] = tuple_element_addr [[TUPLE_ADDR1]] : $*(Int, Int), 1
// CHECK: store [[TMP]] to [trivial] [[KEY_ADDR]] : $*Int
// CHECK: store [[TMP]] to [trivial] [[VALUE_ADDR]] : $*Int
// CHECK: [[FIN_FN:%.*]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
// CHECK: [[FIN_ARR:%.*]] = apply [[FIN_FN]]<(Int, Int)>([[ARR]])
// CHECK: [[METATYPE:%.*]] = metatype $@thick TakesDictionaryLiteral<Int, Int>.Type
// CHECK: [[CTOR:%.*]] = class_method [[METATYPE]] : $@thick TakesDictionaryLiteral<Int, Int>.Type, #TakesDictionaryLiteral.init!allocator : <Key, Value> (TakesDictionaryLiteral<Key, Value>.Type) -> ((Key, Value)...) -> TakesDictionaryLiteral<Key, Value>, $@convention(method) <τ_0_0, τ_0_1> (@owned Array<(τ_0_0, τ_0_1)>, @thick TakesDictionaryLiteral<τ_0_0, τ_0_1>.Type) -> @owned TakesDictionaryLiteral<τ_0_0, τ_0_1>
// CHECK: [[RESULT:%.*]] = apply [[CTOR]]<Int, Int>([[FIN_ARR]], [[METATYPE]])
// CHECK: return [[RESULT]]

func returnsCustomDictionary() -> TakesDictionaryLiteral<Int, Int> {
  // Use temporary to simplify generated_sil
  let tmp = 77
  return [tmp: tmp, tmp : tmp]
}

struct Color: _ExpressibleByColorLiteral {
  init(_colorLiteralRed red: Float, green: Float, blue: Float, alpha: Float) {}
}

// CHECK-LABEL: sil hidden [ossa] @$s8literals16makeColorLiteralAA0C0VyF : $@convention(thin) () -> Color {
// CHECK: [[COLOR_METATYPE:%.*]] = metatype $@thin Color.Type
// CHECK: [[VALUE:%.*]] = float_literal $Builtin.FPIEEE{{64|80}}, {{0x3FF5BA5E353F7CEE|0x3FFFADD2F1A9FBE76C8B}}
// CHECK: [[METATYPE:%.*]] = metatype $@thin Float.Type
// CHECK: [[FN:%.*]] = function_ref @$sSf20_builtinFloatLiteralSfBf{{64|80}}__tcfC
// CHECK: [[R:%.*]] = apply [[FN]]([[VALUE]], [[METATYPE]]) : $@convention(method) (Builtin.FPIEEE{{64|80}}, @thin Float.Type) -> Float

// CHECK: [[VALUE:%.*]] = float_literal $Builtin.FPIEEE{{64|80}}, {{0xBFB2F1A9FBE76C8B|0xBFFB978D4FDF3B645A1D}}
// CHECK: [[METATYPE:%.*]] = metatype $@thin Float.Type
// CHECK: [[FN:%.*]] = function_ref @$sSf20_builtinFloatLiteralSfBf{{64|80}}__tcfC
// CHECK: [[G:%.*]] = apply [[FN]]([[VALUE]], [[METATYPE]]) : $@convention(method) (Builtin.FPIEEE{{64|80}}, @thin Float.Type) -> Float

// CHECK: [[VALUE:%.*]] = float_literal $Builtin.FPIEEE{{64|80}}, {{0xBF889374BC6A7EFA|0xBFF8C49BA5E353F7CED9}}
// CHECK: [[METATYPE:%.*]] = metatype $@thin Float.Type
// CHECK: [[FN:%.*]] = function_ref @$sSf20_builtinFloatLiteralSfBf{{64|80}}__tcfC
// CHECK: [[B:%.*]] = apply [[FN]]([[VALUE]], [[METATYPE]]) : $@convention(method) (Builtin.FPIEEE{{64|80}}, @thin Float.Type) -> Float

// CHECK: [[VALUE:%.*]] = float_literal $Builtin.FPIEEE{{64|80}}, {{0x3FF0000000000000|0x3FFF8000000000000000}}
// CHECK: [[METATYPE:%.*]] = metatype $@thin Float.Type
// CHECK: [[FN:%.*]] = function_ref @$sSf20_builtinFloatLiteralSfBf{{64|80}}__tcfC
// CHECK: [[A:%.*]] = apply [[FN]]([[VALUE]], [[METATYPE]]) : $@convention(method) (Builtin.FPIEEE{{64|80}}, @thin Float.Type) -> Float

// CHECK: [[FN:%.*]] = function_ref @$s8literals5ColorV16_colorLiteralRed5green4blue5alphaACSf_S3ftcfC : $@convention(method) (Float, Float, Float, Float, @thin Color.Type) -> Color
// CHECK: [[LIT:%.*]] = apply [[FN]]([[R]], [[G]], [[B]], [[A]], [[COLOR_METATYPE]]) : $@convention(method) (Float, Float, Float, Float, @thin Color.Type) -> Color
// CHECK: return [[LIT]] : $Color
func makeColorLiteral() -> Color {
  return #colorLiteral(red: 1.358, green: -0.074, blue: -0.012, alpha: 1.0)
}

struct Image: _ExpressibleByImageLiteral {
  init(imageLiteralResourceName: String) {}
}

func makeTmpString() -> String { return "" }

// CHECK-LABEL: sil hidden [ossa] @$s8literals16makeImageLiteralAA0C0VyF : $@convention(thin) () -> Image
// CHECK: [[METATYPE:%.*]] = metatype $@thin Image.Type
// CHECK: [[FN:%.*]] = function_ref @$s8literals13makeTmpStringSSyF : $@convention(thin) () -> @owned String
// CHECK: [[STR:%.*]] = apply [[FN]]() : $@convention(thin) () -> @owned String
// CHECK: [[FN:%.*]] = function_ref @$s8literals5ImageV24imageLiteralResourceNameACSS_tcfC : $@convention(method) (@owned String, @thin Image.Type) -> Image
// CHECK: [[LIT:%.*]] = apply [[FN]]([[STR]], [[METATYPE]]) : $@convention(method) (@owned String, @thin Image.Type) -> Image
// CHECK: return [[LIT]] : $Image
func makeImageLiteral() -> Image {
  return #imageLiteral(resourceName: makeTmpString())
}

struct FileReference: _ExpressibleByFileReferenceLiteral {
  init(fileReferenceLiteralResourceName: String) {}
}

// CHECK-LABEL: sil hidden [ossa] @$s8literals24makeFileReferenceLiteralAA0cD0VyF : $@convention(thin) () -> FileReference
// CHECK: [[METATYPE:%.*]] = metatype $@thin FileReference.Type
// CHECK: [[FN:%.*]] = function_ref @$s8literals13makeTmpStringSSyF : $@convention(thin) () -> @owned String
// CHECK: [[STR:%.*]] = apply [[FN]]() : $@convention(thin) () -> @owned String
// CHECK: [[FN:%.*]] = function_ref @$s8literals13FileReferenceV04fileC19LiteralResourceNameACSS_tcfC
// CHECK: [[LIT:%.*]] = apply [[FN]]([[STR]], [[METATYPE]]) : $@convention(method) (@owned String, @thin FileReference.Type) -> FileReference
// CHECK: return [[LIT]] : $FileReference
func makeFileReferenceLiteral() -> FileReference {
  return #fileLiteral(resourceName: makeTmpString())
}

class ReferenceColor<T> { required init() {} }

protocol Silly {
  init()
  init(_colorLiteralRed red: Float, green: Float, blue: Float, alpha: Float)
}

extension Silly {
  init(_colorLiteralRed red: Float, green: Float, blue: Float, alpha: Float) {
    self.init()
  }
}

extension ReferenceColor : Silly, _ExpressibleByColorLiteral {}

func makeColorLiteral<T>() -> ReferenceColor<T> {
  return #colorLiteral(red: 1.358, green: -0.074, blue: -0.012, alpha: 1.0)
}

// CHECK-LABEL: sil hidden [ossa] @$s8literals16makeColorLiteralAA09ReferenceC0CyxGylF : $@convention(thin) <T> () -> @owned ReferenceColor<T>
// CHECK: [[FN:%.*]] = function_ref @$s8literals5SillyPAAE16_colorLiteralRed5green4blue5alphaxSf_S3ftcfC : $@convention(method) <τ_0_0 where τ_0_0 : Silly> (Float, Float, Float, Float, @thick τ_0_0.Type) -> @out τ_0_0
// CHECK: apply [[FN]]<ReferenceColor<T>>(
