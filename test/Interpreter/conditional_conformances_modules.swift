// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/libBasic.%target-dylib-extension) %S/../Inputs/conditional_conformance_basic_conformances.swift -module-name Basic -emit-module -emit-module-path %t/Basic.swiftmodule
// RUN: %target-build-swift-dylib(%t/libWithAssoc.%target-dylib-extension) %S/../Inputs/conditional_conformance_with_assoc.swift -module-name WithAssoc -emit-module -emit-module-path %t/WithAssoc.swiftmodule
// RUN: %target-build-swift-dylib(%t/libSubclass.%target-dylib-extension) %S/../Inputs/conditional_conformance_subclass.swift -module-name Subclass -emit-module -emit-module-path %t/Subclass.swiftmodule
// RUN: %target-build-swift -I%t -L%t -lBasic -lWithAssoc -lSubclass %s -o %t/conditional_conformances_modules -Xlinker -rpath -Xlinker %t
// RUN: %target-run %t/conditional_conformances_modules %t/libBasic.%target-dylib-extension %t/libWithAssoc.%target-dylib-extension %t/libSubclass.%target-dylib-extension

// REQUIRES: executable_test
// FIXME: seems to fail on 32-bit simulator?
// REQUIRES: OS=macosx || OS=linux

import Basic
import WithAssoc
import Subclass


public func basic_single_generic<T: Basic.P2>(_: T.Type) {
  Basic.takes_p1(Basic.Single<T>.self)
}
public func basic_single_concrete() {
  Basic.takes_p1(Basic.Single<Basic.IsP2>.self)
}
public func basic_double_generic_generic<U: Basic.P2, V: Basic.P3>(_: U.Type, _: V.Type) {
  Basic.takes_p1(Basic.Double<U, V>.self)
}
public func basic_double_generic_concrete<X: Basic.P2>(_: X.Type) {
  Basic.takes_p1(Basic.Double<X, Basic.IsP3>.self)
}
public func basic_double_concrete_concrete() {
  Basic.takes_p1(Basic.Double<Basic.IsP2, Basic.IsP3>.self)
}


public func with_assoc_generic_generic<T: WithAssoc.P2, U>(_: T.Type, _: U.Type)
  where T.AT2: WithAssoc.P2, U: WithAssoc.P3, T.AT2.AT2.AT3: WithAssoc.P3
{
  WithAssoc.takes_p1(WithAssoc.Double<T, U>.self)
}
public func with_assoc_generic_concrete<T: WithAssoc.P2>(_: T.Type)
  where T.AT2: WithAssoc.P2, T.AT2.AT2.AT3: WithAssoc.P3
{
  WithAssoc.takes_p1(WithAssoc.Double<T, WithAssoc.IsP3>.self)
}
public func with_assoc_concrete_generic<U>(_: U.Type)
  where U: WithAssoc.P3
{
  WithAssoc.takes_p1(WithAssoc.Double<WithAssoc.IsAlsoP2, U>.self)
}
public func with_assoc_concrete_concrete() {
  WithAssoc.takes_p1(WithAssoc.Double<WithAssoc.IsAlsoP2, WithAssoc.IsP3>.self)
}

public func subclass_subclassgeneric_generic<T: Subclass.P2>(_: T.Type) {
  Subclass.takes_p1(Subclass.SubclassGeneric<T>.self)
}
public func subclass_subclassgeneric_concrete() {
  Subclass.takes_p1(Subclass.SubclassGeneric<Subclass.IsP2>.self)
}
public func subclass_subclassconcrete() {
  Subclass.takes_p1(Subclass.SubclassConcrete.self)
}
public func subclass_subclassgenericconcrete() {
  Subclass.takes_p1(Subclass.SubclassGenericConcrete.self)
}


basic_single_generic(Basic.IsP2.self)
basic_single_concrete()
basic_double_generic_generic(Basic.IsP2.self, Basic.IsP3.self)
basic_double_generic_concrete(Basic.IsP2.self)
basic_double_concrete_concrete()


with_assoc_generic_generic(WithAssoc.IsAlsoP2.self, WithAssoc.IsP3.self)
with_assoc_generic_concrete(WithAssoc.IsAlsoP2.self)
with_assoc_concrete_generic(WithAssoc.IsP3.self)
with_assoc_concrete_concrete()


subclass_subclassgeneric_generic(Subclass.IsP2.self)
subclass_subclassgeneric_concrete()
subclass_subclassconcrete()
subclass_subclassgenericconcrete()
