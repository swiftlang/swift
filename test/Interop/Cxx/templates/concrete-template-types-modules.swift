// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/library.swift -module-name Concrete -o %t/Concrete.swiftmodule -I %S/Inputs/concrete-template-types -cxx-interoperability-mode=default -enable-experimental-feature CxxConcreteTemplateTypes
// RUN: %target-swift-frontend -typecheck %t/client.swift -I %t -I %S/Inputs/concrete-template-types -cxx-interoperability-mode=default -verify
// RUN: %target-swift-frontend -typecheck %t/extra.swift -I %S/Inputs/concrete-template-types -cxx-interoperability-mode=default -enable-experimental-feature CxxConcreteTemplateTypes -verify
// The disabled run also emits an availability note in types.h.
// RUN: %target-swift-frontend -typecheck %t/disabled.swift -I %S/Inputs/concrete-template-types -cxx-interoperability-mode=default -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/library.swift -module-name Concrete -emit-clang-header-path %t/Concrete.h -I %S/Inputs/concrete-template-types -cxx-interoperability-mode=default -enable-experimental-feature CxxConcreteTemplateTypes
// RUN: %target-interop-build-clangxx -std=c++17 -fsyntax-only %t/client.cpp -I %t -I %S/Inputs/concrete-template-types
// RUN: %target-swift-frontend -typecheck %t/hidden.swift -I %S/Inputs/concrete-template-types -cxx-interoperability-mode=default -enable-experimental-feature CxxConcreteTemplateTypes -verify
// RUN: %target-swift-frontend -typecheck %t/visible.swift -I %S/Inputs/concrete-template-types -cxx-interoperability-mode=default -enable-experimental-feature CxxConcreteTemplateTypes -verify
// RUN: %target-swift-frontend -typecheck %t/library.swift -module-name Concrete -emit-module-interface-path %t/Concrete.swiftinterface -swift-version 5 -enable-library-evolution -enable-experimental-feature AssumeResilientCxxTypes -I %S/Inputs/concrete-template-types -cxx-interoperability-mode=default -enable-experimental-feature CxxConcreteTemplateTypes
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/Concrete.swiftinterface -module-name Concrete -I %S/Inputs/concrete-template-types -cxx-interoperability-mode=default
// REQUIRES: swift_feature_CxxConcreteTemplateTypes
// REQUIRES: swift_feature_AssumeResilientCxxTypes

//--- library.swift
import ConcreteTemplateTypes
import AdditionalTemplateTypes
public typealias PointerResult = concrete.Result<UnsafeMutableRawPointer>
public typealias EmptyResult = concrete.Result<Void>
public typealias DoubleResult = concrete.Result<CDouble>
public func makeDouble() -> DoubleResult { DoubleResult(value: 2.5) }
public func pointer(_ value: concrete.Result<UnsafeMutableRawPointer>) -> concrete.Result<UnsafeMutableRawPointer> { value }
public func empty(_ value: concrete.Result<Void>) -> concrete.Result<Void> { value }

//--- client.swift
import Concrete
import ConcreteTemplateTypes
func check() {
  let p: PointerResult = pointer(concrete.makePointer())
  let v: EmptyResult = empty(concrete.makeVoid())
  _ = pointer(p)
  _ = empty(v)
  let _: DoubleResult = makeDouble()
}

//--- extra.swift
import ConcreteTemplateTypes
import AdditionalTemplateTypes
func additional(_ value: concrete.Result<CDouble>) -> concrete.Result<CDouble> { value }

//--- disabled.swift
import ConcreteTemplateTypes
func pointer(_ value: concrete.Result<UnsafeMutableRawPointer>) {} // expected-error {{'Result' is unavailable}}

//--- client.cpp
#include "additional.h"
#include "Concrete.h"
#include <type_traits>
static_assert(std::is_same<decltype(Concrete::pointer(concrete::makePointer())),
                           concrete::Result<void *>>::value, "pointer identity");
static_assert(std::is_same<decltype(Concrete::empty(concrete::makeVoid())),
                           concrete::Result<void>>::value, "void identity");

//--- hidden.swift
import TemplateVisibility.Base
func base(_ value: visibility.Box<CInt>) {}
func hidden(_ value: visibility.Box<CDouble>) {} // expected-error {{no complete, visible, explicitly declared C++ specialization of 'Box' matches these type arguments}}

//--- visible.swift
import TemplateVisibility.Extra
import TemplateVisibility.Base
func visible(_ value: visibility.Box<CDouble>) {}
