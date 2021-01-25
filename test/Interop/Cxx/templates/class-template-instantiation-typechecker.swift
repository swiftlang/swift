// RUN: not %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop 2>&1 | %FileCheck %s

import ClassTemplateInstantiationErrors

// CHECK: class-template-instantiation-typechecker.swift:7:11: error: could not generate C++ types from the generic Swift types provided. The following Swift type(s) provided to 'MagicWrapper' could not be converted: Optional<_>.
func swiftTemplateArgNotSupported() {
  var _ = MagicWrapper<Optional>(t: "asdf")
}

// CHECK: class-template-instantiation-errors.h:18:7: error: no member named 'doesNotExist' in 'IntWrapper'
// CHECK: class-template-instantiation-errors.h:16:8: note: in instantiation of member function 'CannotBeInstantianted<IntWrapper>::willFailInstantiating' requested here
func clangErrorReportedOnInstantiation() {
    var _ = CannotBeInstantianted<IntWrapper>()
}