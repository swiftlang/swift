// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir %s

// Protocols with superclass-constrained Self.

class Concrete {
  typealias ConcreteAlias = String

  func concreteMethod(_: ConcreteAlias) {}
}

class Generic<T> : Concrete {
  typealias GenericAlias = (T, T)

  func genericMethod(_: GenericAlias) {}
}

protocol BaseProto {}

protocol ProtoRefinesClass : Generic<Int>, BaseProto {
  func requirementUsesClassTypes(_: ConcreteAlias, _: GenericAlias)
}

extension ProtoRefinesClass {
  // CHECK-LABEL: sil hidden [ossa] @$s24protocol_with_superclass17ProtoRefinesClassPAAE019extensionMethodUsesF5TypesyySS_Si_SittF : $@convention(method) <Self where Self : ProtoRefinesClass> (@guaranteed String, Int, Int, @guaranteed Self) -> ()
  func extensionMethodUsesClassTypes(_ x: ConcreteAlias, _ y: GenericAlias) {
    _ = ConcreteAlias.self
    _ = GenericAlias.self

    // CHECK:      [[SELF:%.*]] = copy_value %3 : $Self
    // CHECK-NEXT: [[UPCAST:%.*]] = upcast [[SELF]] : $Self to $Generic<Int>
    // CHECK-NEXT: [[UPCAST2:%.*]] = upcast [[UPCAST]] : $Generic<Int> to $Concrete
    // CHECK-NEXT: [[METHOD:%.*]] = class_method [[UPCAST2]] : $Concrete, #Concrete.concreteMethod : (Concrete) -> (String) -> (), $@convention(method) (@guaranteed String, @guaranteed Concrete) -> ()
    // CHECK-NEXT: apply [[METHOD]](%0, [[UPCAST2]])
    // CHECK-NEXT: destroy_value [[UPCAST2]]
    concreteMethod(x)

    // CHECK:      [[SELF:%.*]] = copy_value %3 : $Self
    // CHECK-NEXT: [[UPCAST:%.*]] = upcast [[SELF]] : $Self to $Generic<Int>
    // CHECK:      [[METHOD:%.*]] = class_method [[UPCAST]] : $Generic<Int>, #Generic.genericMethod : <T> (Generic<T>) -> ((T, T)) -> (), $@convention(method) <τ_0_0> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @guaranteed Generic<τ_0_0>) -> ()
    // CHECK-NEXT: apply [[METHOD]]<Int>({{.*}}, [[UPCAST]])
    // CHECK-NEXT: dealloc_stack
    // CHECK-NEXT: dealloc_stack
    // CHECK-NEXT: destroy_value [[UPCAST]]
    genericMethod(y)

    // CHECK:      [[SELF:%.*]] = copy_value %3 : $Self
    // CHECK-NEXT: [[UPCAST:%.*]] = upcast [[SELF]] : $Self to $Generic<Int>
    // CHECK-NEXT: ignored_use
    // CHECK-NEXT: destroy_value [[UPCAST]] : $Generic<Int>
    let _: Generic<Int> = self

    // CHECK:      [[SELF:%.*]] = copy_value %3 : $Self
    // CHECK-NEXT: [[UPCAST:%.*]] = upcast [[SELF]] : $Self to $Generic<Int>
    // CHECK-NEXT: [[UPCAST2:%.*]] = upcast [[UPCAST]] : $Generic<Int> to $Concrete
    // CHECK-NEXT: ignored_use
    // CHECK-NEXT: destroy_value [[UPCAST2]] : $Concrete
    let _: Concrete = self

    // CHECK:      [[BOX:%.*]] = alloc_stack $any BaseProto
    // CHECK-NEXT: [[SELF:%.*]] = copy_value %3 : $Self
    // CHECK-NEXT: [[ADDR:%.*]] = init_existential_addr [[BOX]] : $*any BaseProto, $Self
    // CHECK-NEXT: store [[SELF]] to [init] [[ADDR]] : $*Self
    // CHECK-NEXT: ignored_use
    // CHECK-NEXT: destroy_addr [[BOX]] : $*any BaseProto
    // CHECK-NEXT: dealloc_stack [[BOX]] : $*any BaseProto
    let _: BaseProto = self

    // CHECK:      [[SELF:%.*]] = copy_value %3 : $Self
    // CHECK-NEXT: [[EXISTENTIAL:%.*]] = init_existential_ref [[SELF]] : $Self : $Self, $any Generic<Int> & BaseProto
    let _: BaseProto & Generic<Int> = self
    
    // CHECK:      [[SELF:%.*]] = copy_value %3 : $Self
    // CHECK-NEXT: [[EXISTENTIAL:%.*]] = init_existential_ref [[SELF]] : $Self : $Self, $any Concrete & BaseProto
    let _: BaseProto & Concrete = self

    // CHECK:      return
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s24protocol_with_superclass22usesProtoRefinesClass1yyAA0eF5Class_pF : $@convention(thin) (@guaranteed any ProtoRefinesClass) -> ()
func usesProtoRefinesClass1(_ t: ProtoRefinesClass) {
  let x: ProtoRefinesClass.ConcreteAlias = "hi"
  _ = ProtoRefinesClass.ConcreteAlias.self

  t.concreteMethod(x)

  let y: ProtoRefinesClass.GenericAlias = (1, 2)
  _ = ProtoRefinesClass.GenericAlias.self

  t.genericMethod(y)

  t.requirementUsesClassTypes(x, y)

  let _: Generic<Int> = t
  let _: Concrete = t
  let _: BaseProto = t
  let _: BaseProto & Generic<Int> = t
  let _: BaseProto & Concrete = t
}

// CHECK-LABEL: sil hidden [ossa] @$s24protocol_with_superclass22usesProtoRefinesClass2yyxAA0eF5ClassRzlF : $@convention(thin) <T where T : ProtoRefinesClass> (@guaranteed T) -> ()
func usesProtoRefinesClass2<T : ProtoRefinesClass>(_ t: T) {
  let x: T.ConcreteAlias = "hi"
  _ = T.ConcreteAlias.self

  t.concreteMethod(x)

  let y: T.GenericAlias = (1, 2)
  _ = T.GenericAlias.self

  t.genericMethod(y)

  t.requirementUsesClassTypes(x, y)

  let _: Generic<Int> = t
  let _: Concrete = t
  let _: BaseProto = t
  let _: BaseProto & Generic<Int> = t
  let _: BaseProto & Concrete = t
}

class GoodConformingClass : Generic<Int>, ProtoRefinesClass {
  // CHECK-LABEL: sil hidden [ossa] @$s24protocol_with_superclass19GoodConformingClassC015requirementUsesF5TypesyySS_Si_SittF : $@convention(method) (@guaranteed String, Int, Int, @guaranteed GoodConformingClass) -> ()
  func requirementUsesClassTypes(_ x: ConcreteAlias, _ y: GenericAlias) {
    _ = ConcreteAlias.self
    _ = GenericAlias.self

    concreteMethod(x)

    genericMethod(y)
  }
}

protocol ProtoRefinesProtoWithClass : ProtoRefinesClass {}

extension ProtoRefinesProtoWithClass {
  // CHECK-LABEL: sil hidden [ossa] @$s24protocol_with_superclass012ProtoRefinesD9WithClassPAAE026anotherExtensionMethodUsesG5TypesyySS_Si_SittF : $@convention(method) <Self where Self : ProtoRefinesProtoWithClass> (@guaranteed String, Int, Int, @guaranteed Self) -> () 
  func anotherExtensionMethodUsesClassTypes(_ x: ConcreteAlias, _ y: GenericAlias) {
    _ = ConcreteAlias.self
    _ = GenericAlias.self

    concreteMethod(x)
    genericMethod(y)

    let _: Generic<Int> = self
    let _: Concrete = self
    let _: BaseProto = self
    let _: BaseProto & Generic<Int> = self
    let _: BaseProto & Concrete = self
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s24protocol_with_superclass016usesProtoRefinesE10WithClass1yyAA0efeG5Class_pF : $@convention(thin) (@guaranteed any ProtoRefinesProtoWithClass) -> ()
func usesProtoRefinesProtoWithClass1(_ t: ProtoRefinesProtoWithClass) {
  let x: ProtoRefinesProtoWithClass.ConcreteAlias = "hi"
  _ = ProtoRefinesProtoWithClass.ConcreteAlias.self

  t.concreteMethod(x)

  let y: ProtoRefinesProtoWithClass.GenericAlias = (1, 2)
  _ = ProtoRefinesProtoWithClass.GenericAlias.self

  t.genericMethod(y)

  t.requirementUsesClassTypes(x, y)

  let _: Generic<Int> = t
  let _: Concrete = t
  let _: BaseProto = t
  let _: BaseProto & Generic<Int> = t
  let _: BaseProto & Concrete = t
}

// CHECK-LABEL: sil hidden [ossa] @$s24protocol_with_superclass016usesProtoRefinesE10WithClass2yyxAA0efeG5ClassRzlF : $@convention(thin) <T where T : ProtoRefinesProtoWithClass> (@guaranteed T) -> ()
func usesProtoRefinesProtoWithClass2<T : ProtoRefinesProtoWithClass>(_ t: T) {
  let x: T.ConcreteAlias = "hi"
  _ = T.ConcreteAlias.self

  t.concreteMethod(x)

  let y: T.GenericAlias = (1, 2)
  _ = T.GenericAlias.self

  t.genericMethod(y)

  t.requirementUsesClassTypes(x, y)

  let _: Generic<Int> = t
  let _: Concrete = t
  let _: BaseProto = t
  let _: BaseProto & Generic<Int> = t
  let _: BaseProto & Concrete = t
}

class ClassWithInits<T> {
  init(notRequiredInit: ()) {}

  required init(requiredInit: ()) {}
}

protocol ProtocolWithClassInits : ClassWithInits<Int> {}

// CHECK-LABEL: sil hidden [ossa] @$s24protocol_with_superclass26useProtocolWithClassInits1yyAA0efG5Inits_pXpF : $@convention(thin) (@thick any ProtocolWithClassInits.Type) -> ()
func useProtocolWithClassInits1(_ t: ProtocolWithClassInits.Type) {
  // CHECK: [[OPENED:%.*]] = open_existential_metatype %0 : $@thick any ProtocolWithClassInits.Type
  // CHECK-NEXT: [[UPCAST:%.*]] = upcast [[OPENED]] : $@thick (@opened("{{.*}}", any ProtocolWithClassInits) Self).Type to $@thick ClassWithInits<Int>.Type
  // CHECK-NEXT: [[METHOD:%.*]] = class_method [[UPCAST]] : $@thick ClassWithInits<Int>.Type, #ClassWithInits.init!allocator : <T> (ClassWithInits<T>.Type) -> (()) -> ClassWithInits<T>, $@convention(method) <τ_0_0> (@thick ClassWithInits<τ_0_0>.Type) -> @owned ClassWithInits<τ_0_0>
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]<Int>([[UPCAST]])
  // CHECK-NEXT: [[CAST:%.*]] = unchecked_ref_cast [[RESULT]] : $ClassWithInits<Int> to $@opened("{{.*}}", any ProtocolWithClassInits) Self
  // CHECK-NEXT: [[EXISTENTIAL:%.*]] = init_existential_ref [[CAST]] : $@opened("{{.*}}", any ProtocolWithClassInits) Self : $@opened("{{.*}}", any ProtocolWithClassInits) Self, $any ProtocolWithClassInits
  // CHECK-NEXT: ignored_use
  // CHECK-NEXT: destroy_value [[EXISTENTIAL]]
  let _: ProtocolWithClassInits = t.init(requiredInit: ())
}

// CHECK-LABEL: sil hidden [ossa] @$s24protocol_with_superclass26useProtocolWithClassInits2yyxmAA0efG5InitsRzlF : $@convention(thin) <T where T : ProtocolWithClassInits> (@thick T.Type) -> ()
func useProtocolWithClassInits2<T : ProtocolWithClassInits>(_ t: T.Type) {
  let _: T = T(requiredInit: ())

  let _: T = t.init(requiredInit: ())
}

protocol ProtocolRefinesClassInits : ProtocolWithClassInits {}

// CHECK-LABEL: sil hidden [ossa] @$s24protocol_with_superclass29useProtocolRefinesClassInits1yyAA0efG5Inits_pXpF : $@convention(thin) (@thick any ProtocolRefinesClassInits.Type) -> ()
func useProtocolRefinesClassInits1(_ t: ProtocolRefinesClassInits.Type) {
  let _: ProtocolRefinesClassInits = t.init(requiredInit: ())
}

// CHECK-LABEL: sil hidden [ossa] @$s24protocol_with_superclass29useProtocolRefinesClassInits2yyxmAA0efG5InitsRzlF : $@convention(thin) <T where T : ProtocolRefinesClassInits> (@thick T.Type) -> ()
func useProtocolRefinesClassInits2<T : ProtocolRefinesClassInits>(_ t: T.Type) {
  let _: T = T(requiredInit: ())

  let _: T = t.init(requiredInit: ())
}

class ClassWithDefault<T> {
  func makeT() -> T { while true {} }
}

protocol SillyDefault : ClassWithDefault<Int> {
  func makeT() -> Int
}

class ConformsToSillyDefault : ClassWithDefault<Int>, SillyDefault {}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s24protocol_with_superclass22ConformsToSillyDefaultCAA0fG0A2aDP5makeTSiyFTW :
// CHECK: class_method %1 : $ClassWithDefault<Int>, #ClassWithDefault.makeT : <T> (ClassWithDefault<T>) -> () -> T, $@convention(method) <τ_0_0> (@guaranteed ClassWithDefault<τ_0_0>) -> @out τ_0_0
// CHECK: return

class BaseClass : BaseProto {}

protocol RefinedProto : BaseClass {}

func takesBaseProtocol(_: BaseProto) {}

func passesRefinedProtocol(_ r: RefinedProto) {
  takesBaseProtocol(r)
}

// CHECK-LABEL: sil hidden [ossa] @$s24protocol_with_superclass21passesRefinedProtocolyyAA0E5Proto_pF : $@convention(thin) (@guaranteed any RefinedProto) -> ()
// CHECK:     bb0(%0 : @guaranteed $any RefinedProto):
// CHECK:       [[OPENED:%.*]] = open_existential_ref %0 : $any RefinedProto to $@opened("{{.*}}", any RefinedProto) Self
// CHECK-NEXT:  [[BASE:%.*]] = alloc_stack $any BaseProto
// CHECK-NEXT:  [[BASE_PAYLOAD:%.*]] = init_existential_addr [[BASE]] : $*any BaseProto, $@opened("{{.*}}", any RefinedProto) Self
// CHECK-NEXT:  [[OPENED_COPY:%.*]] = copy_value [[OPENED]] : $@opened("{{.*}}", any RefinedProto) Self
// CHECK-NEXT:  store [[OPENED_COPY]] to [init] [[BASE_PAYLOAD]] : $*@opened("{{.*}}", any RefinedProto) Self
// CHECK:       [[FUNC:%.*]] = function_ref @$s24protocol_with_superclass17takesBaseProtocolyyAA0E5Proto_pF : $@convention(thin) (@in_guaranteed any BaseProto) -> ()
// CHECK-NEXT:  apply [[FUNC]]([[BASE]]) : $@convention(thin) (@in_guaranteed any BaseProto) -> ()
// CHECK-NEXT:  destroy_addr [[BASE]] : $*any BaseProto
// CHECK-NEXT:  dealloc_stack [[BASE]] : $*any BaseProto
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]] : $()

func takesFuncTakingRefinedProto(arg: (RefinedProto) -> ()) {}

func passesFuncTakingBaseClass() {
  let closure: (BaseClass) -> () = { _ in }
  takesFuncTakingRefinedProto(arg: closure)
}

// CHECK-LABEL: sil hidden [ossa] @$s24protocol_with_superclass25passesFuncTakingBaseClassyyF : $@convention(thin) () -> ()

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s24protocol_with_superclass9BaseClassCIegg_AA12RefinedProto_pIegg_TR : $@convention(thin) (@guaranteed any RefinedProto, @guaranteed @callee_guaranteed (@guaranteed BaseClass) -> ()) -> ()
// CHECK: [[PAYLOAD:%.*]] = open_existential_ref %0 : $any RefinedProto to $@opened("{{.*}}", any RefinedProto) Self
// CHECK: [[COPY:%.*]] = copy_value [[PAYLOAD]]
// CHECK: [[UPCAST:%.*]] = upcast [[COPY]] : $@opened("{{.*}}", any RefinedProto) Self to $BaseClass
// CHECK: [[BORROW:%.*]] = begin_borrow [[UPCAST]]
// CHECK: apply %1([[BORROW]])
// CHECK: end_borrow [[BORROW]]
// CHECK: destroy_value [[UPCAST]]
// CHECK: return

// CHECK-LABEL: sil_witness_table hidden ConformsToSillyDefault: SillyDefault module protocol_with_superclass {
// CHECK-NEXT: method #SillyDefault.makeT: <Self where Self : SillyDefault> (Self) -> () -> Int : @$s24protocol_with_superclass22ConformsToSillyDefaultCAA0fG0A2aDP5makeTSiyFTW
// CHECK-NEXT: }
