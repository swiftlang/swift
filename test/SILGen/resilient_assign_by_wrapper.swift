// RUN: %target-swift-emit-silgen %s -emit-verbose-sil -enable-library-evolution | %FileCheck %s

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

  func testAssignment() {
    // CHECK-LABEL: sil hidden [ossa] @$s27resilient_assign_by_wrapper17AddressOnlySetterC14testAssignmentyyF
    // CHECK: [[E:%.*]] = alloc_stack $AddressOnlyEnum
    // CHECK: inject_enum_addr [[E]] : $*AddressOnlyEnum, #AddressOnlyEnum.some!enumelt
    // CHECK: [[S:%.*]] = partial_apply [callee_guaranteed] {{%.*}}({{%.*}}) : $@convention(method) (@in AddressOnlyEnum, @guaranteed AddressOnlySetter) -> ()
    // CHECK: assign_by_wrapper [[E]] : $*AddressOnlyEnum
    // CHECK-SAME: set [[S]] : $@callee_guaranteed (@in AddressOnlyEnum) -> ()
    self.value = .some
  }
}
