// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IValue {}

@com(interface: "10000000-0000-0000-0000-000000000002")
protocol IClassValue: IValue, AnyObject {}

func invalidReferences(_ value: any IValue, _ classValue: any IClassValue) {
  weak var weakValue: (any IValue)? = value
  // expected-error@-1 {{'any IValue' is incompatible with 'weak' references}}
  unowned var unownedValue: any IValue = value
  // expected-error@-1 {{'any IValue' is incompatible with 'unowned' references}}
  unowned var optionalValue: (any IValue)? = value
  // expected-error@-1 {{'any IValue' is incompatible with 'unowned' references}}

  // A class bound does not make the interface pointer a Swift object pointer.
  weak var weakClassValue: (any IClassValue)? = classValue
  // expected-error@-1 {{'any IClassValue' is incompatible with 'weak' references}}
  unowned var unownedClassValue: any IClassValue = classValue
  // expected-error@-1 {{'any IClassValue' is incompatible with 'unowned' references}}
  unowned var optionalClassValue: (any IClassValue)? = classValue
  // expected-error@-1 {{'any IClassValue' is incompatible with 'unowned' references}}

  weakValue = nil
  unownedValue = value
  optionalValue = nil
  weakClassValue = nil
  unownedClassValue = classValue
  optionalClassValue = nil
  _ = (weakValue, unownedValue, optionalValue,
       weakClassValue, unownedClassValue, optionalClassValue)

  _ = { [weak value] in
    // expected-error@-1 {{'any IValue' is incompatible with 'weak' references}}
    value = nil
    return value
  }
  _ = { [unowned value] in value }
  // expected-error@-1 {{'any IValue' is incompatible with 'unowned' references}}
  _ = { [weak classValue] in
    // expected-error@-1 {{'any IClassValue' is incompatible with 'weak' references}}
    classValue = nil
    return classValue
  }
  _ = { [unowned classValue] in classValue }
  // expected-error@-1 {{'any IClassValue' is incompatible with 'unowned' references}}
}

// Native object references still use Swift reference storage, even when the
// class implements a COM interface.
@com(implementation: "10000000-0000-0000-0000-000000000003")
final class NativeValue: IValue {}

final class NativeHolder {
  weak var weakValue: NativeValue?
  unowned var unownedValue: NativeValue
  unowned(unsafe) var unmanagedValue: NativeValue

  init(_ value: NativeValue) {
    weakValue = value
    unownedValue = value
    unmanagedValue = value
  }
}

final class InterfaceHolder {
  unowned(unsafe) var value: any IValue
  unowned(unsafe) var optional: (any IValue)?
  unowned(unsafe) var classValue: any IClassValue
  unowned(unsafe) var optionalClassValue: (any IClassValue)?

  init(_ value: any IValue, _ classValue: any IClassValue) {
    self.value = value
    self.optional = value
    self.classValue = classValue
    self.optionalClassValue = classValue
  }
}
