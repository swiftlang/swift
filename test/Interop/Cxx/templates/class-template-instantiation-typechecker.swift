// RUN: not %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop 2>&1 | %FileCheck %s

import ClassTemplateInstantiationErrors

// CHECK: class-template-instantiation-typechecker.swift:7:11: error: could not generate C++ types from the generic Swift types provided; the following Swift type(s) provided to 'MagicWrapper' could not be converted: Optional<_>
func swiftTemplateArgNotSupported() {
  var _ = MagicWrapper<Optional>(t: "asdf")
}

// CHECK: class-template-instantiation-typechecker.swift:12:11: error: could not generate C++ types from the generic Swift types provided; the following Swift type(s) provided to 'MagicWrapperWithExplicitCtor' could not be converted: String
func swiftTemplateArgNotSupportedExplicitCtor() {
  var _ = MagicWrapperWithExplicitCtor<String>("asdf")
}
