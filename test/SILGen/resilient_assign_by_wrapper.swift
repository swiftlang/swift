// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s -emit-verbose-sil -enable-library-evolution | %FileCheck %s

@propertyWrapper
public struct WrapGod<T> {
  private var value: T

  public init(wrappedValue: T) {
    value = wrappedValue
  }

  public var wrappedValue: T {
    get { value }
    set { value = newValue }
  }
}

public protocol Existential {}

public enum AddressOnlyEnum {
  case some
  case value(Existential?)
}

public class AddressOnlySetter {
  @WrapGod var value: AddressOnlyEnum = .value(nil)

  init() {
    // CHECK-LABEL: sil hidden [ossa] @$s27resilient_assign_by_wrapper17AddressOnlySetterCACycfc
    // CHECK: [[E1:%.*]] = alloc_stack $AddressOnlyEnum
    // CHECK: [[W:%.*]] = alloc_stack $WrapGod<AddressOnlyEnum>
    // CHECK: [[I:%.*]] = function_ref @$s27resilient_assign_by_wrapper17AddressOnlySetterC5valueAA0eF4EnumOvpfP : $@convention(thin) (@in AddressOnlyEnum) -> @out WrapGod<AddressOnlyEnum>
    // CHECK: apply [[I]]([[W]], [[E1]])

    // CHECK: [[E2:%.*]] = alloc_stack $AddressOnlyEnum
    // CHECK-NEXT: inject_enum_addr [[E2]] : $*AddressOnlyEnum, #AddressOnlyEnum.some!enumelt
    // CHECK: [[S:%.*]] = partial_apply [callee_guaranteed] [on_stack] {{%.*}}({{%.*}}) : $@convention(method) (@in AddressOnlyEnum, @guaranteed AddressOnlySetter) -> ()
    // CHECK: assign_by_wrapper [[E2]] : $*AddressOnlyEnum
    // CHECK-SAME: set [[S]] : $@noescape @callee_guaranteed (@in AddressOnlyEnum) -> ()
    self.value = .some
  }

  func testAssignment() {
    // CHECK-LABEL: sil hidden [ossa] @$s27resilient_assign_by_wrapper17AddressOnlySetterC14testAssignmentyyF
    // CHECK: [[E:%.*]] = alloc_stack $AddressOnlyEnum
    // CHECK: inject_enum_addr [[E]] : $*AddressOnlyEnum, #AddressOnlyEnum.some!enumelt
    // CHECK: [[S:%.*]] = class_method %0 : $AddressOnlySetter, #AddressOnlySetter.value!setter : (AddressOnlySetter) -> (AddressOnlyEnum) -> (), $@convention(method) (@in AddressOnlyEnum, @guaranteed AddressOnlySetter) -> ()
    // CHECK: apply [[S]]([[E]], %0) : $@convention(method) (@in AddressOnlyEnum, @guaranteed AddressOnlySetter) -> ()
    self.value = .some
  }
}

public struct SubstitutedSetter<T> {
  @WrapGod var value: T
}

extension SubstitutedSetter where T == Bool {
  init() {
    // CHECK-LABEL: hidden [ossa] @$s27resilient_assign_by_wrapper17SubstitutedSetterVAASbRszlEACySbGycfC
    // CHECK: [[W:%.*]] = struct_element_addr {{%.*}} : $*SubstitutedSetter<Bool>, #SubstitutedSetter._value
    // CHECK: [[B:%.*]] = alloc_stack $Bool
    // CHECK: assign_by_wrapper [[B]] : $*Bool to [[W]] : $*WrapGod<Bool>
    // CHECK-SAME: init {{%.*}} : $@callee_guaranteed (@in Bool) -> @out WrapGod<Bool>
    // CHECK-SAME: set {{%.*}} : $@noescape @callee_guaranteed (@in Bool) -> ()
    self.value = true
  }
}

public struct ReabstractedSetter<T> {
  @WrapGod var value: (T) -> ()
}

extension ReabstractedSetter where T == Int {
  init() {
    // CHECK-LABEL: hidden [ossa] @$s27resilient_assign_by_wrapper18ReabstractedSetterVAASiRszlEACySiGycfC
    // CHECK: [[F:%.*]] = function_ref @$s27resilient_assign_by_wrapper18ReabstractedSetterVAASiRszlEACySiGycfcySicfU_ : $@convention(thin) (Int) -> ()
    // CHECK: [[TH_F:%.*]] = thin_to_thick_function [[F]] : $@convention(thin) (Int) -> () to $@callee_guaranteed (Int) -> ()
    // CHECK: [[THUNK_REF:%.*]] = function_ref @$sSiIegy_SiIegn_TR : $@convention(thin) (@in_guaranteed Int, @guaranteed @callee_guaranteed (Int) -> ()) -> ()
    // CHECK: [[CF:%.*]] = partial_apply [callee_guaranteed] [[THUNK_REF]]([[TH_F]]) : $@convention(thin) (@in_guaranteed Int, @guaranteed @callee_guaranteed (Int) -> ()) -> ()
    // CHECK: [[CF2:%.*]] = convert_function [[CF]]
    // CHECK: assign_by_wrapper [[CF2]]
    // CHECK-SAME: to {{%.*}} : $*WrapGod<(Int) -> ()>
    self.value = { x in }
  }
}

public struct ObjectifiedSetter<T: AnyObject> {
  @WrapGod var value: T
}

public class SomeObject {}

extension ObjectifiedSetter where T == SomeObject {
  init() {
    // CHECK-LABEL: sil hidden [ossa] @$s27resilient_assign_by_wrapper17ObjectifiedSetterV5valuexvs : $@convention(method) <T where T : AnyObject> (@owned T, @inout ObjectifiedSetter<T>) -> () {
    // CHECK: [[OBJ:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@thick SomeObject.Type) -> @owned SomeObject
    // CHECK: [[STORAGE:%.*]] = struct_element_addr {{%.*}} : $*ObjectifiedSetter<SomeObject>, #ObjectifiedSetter._value
    // CHECK: assign_by_wrapper [[OBJ]] : $SomeObject to [[STORAGE]] : $*WrapGod<SomeObject>
    // CHECK-SAME: init {{%.*}} : $@callee_guaranteed (@owned SomeObject) -> @out WrapGod<SomeObject>
    // CHECK-SAME: set {{%.*}} : $@noescape @callee_guaranteed (@owned SomeObject) -> ()
    self.value = SomeObject()
  }
}

