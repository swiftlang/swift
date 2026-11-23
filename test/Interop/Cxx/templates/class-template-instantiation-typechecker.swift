// RUN: not %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -typecheck %s -I %S/Inputs -enable-experimental-cxx-interop 2>&1 | %FileCheck %s --check-prefix=NONTYPE

import ClassTemplateInstantiationErrors
import ClassTemplateNonTypeParameter

// CHECK: class-template-instantiation-typechecker.swift:9:11: error: could not generate C++ types from the generic Swift types provided; the following Swift type(s) provided to 'MagicWrapper' could not be converted: Optional<_>
func swiftTemplateArgNotSupported() {
  var _ = MagicWrapper<Optional>(t: "asdf")
}

// CHECK: class-template-instantiation-typechecker.swift:14:11: error: could not generate C++ types from the generic Swift types provided; the following Swift type(s) provided to 'MagicWrapperWithExplicitCtor' could not be converted: String
func swiftTemplateArgNotSupportedExplicitCtor() {
  var _ = MagicWrapperWithExplicitCtor<String>("asdf")
}

// Only type template parameters can be substituted from Swift.
// NONTYPE: class-template-instantiation-typechecker.swift:[[@LINE+2]]:10: error: could not generate C++ types from the generic Swift types provided; the following Swift type(s) provided to 'MagicArray' could not be converted: CInt
func swiftArgForNonTypeTemplateParam() {
  let _: MagicArray<CInt, CInt>? = nil
}

// CHECK: error: no member named 'doesNotExist' in 'IntWrapper'
// CHECK: note: in instantiation of member function 'CannotBeInstantianted<IntWrapper>::CannotBeInstantianted' requested here

// CHECK: error: no member named 'doesNotExist' in 'IntWrapper'
// CHECK: note: in instantiation of member function 'CannotBeInstantianted<IntWrapper>::memberWrongType' requested here

// CHECK: error: no member named 'doesNotExist' in 'IntWrapper'
// CHECK: note: in instantiation of member function 'CannotBeInstantianted<IntWrapper>::argWrongType' requested here
public func clangErrorReportedOnInstantiation() {
  _ = CannotBeInstantianted<IntWrapper>(CChar(0), IntWrapper())
  var z = CannotBeInstantianted<IntWrapper>(IntWrapper())
  z.memberWrongType()
  z.argWrongType(IntWrapper())
}
