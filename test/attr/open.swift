// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module %S/Inputs/OpenHelpers.swift -o %t/OpenHelpers.swiftmodule
// RUN: %target-typecheck-verify-swift -I %t

import OpenHelpers

/**** General structural limitations on open. ****/

open private class OpenIsNotCompatibleWithPrivate {} // expected-error {{multiple incompatible access-level modifiers specified}} expected-note{{previous modifier specified here}}
open fileprivate class OpenIsNotCompatibleWithFilePrivate {} // expected-error {{multiple incompatible access-level modifiers specified}} expected-note{{previous modifier specified here}}
open internal class OpenIsNotCompatibleWithInternal {} // expected-error {{multiple incompatible access-level modifiers specified}} expected-note{{previous modifier specified here}}
open public class OpenIsNotCompatibleWithPublic {} // expected-error {{multiple incompatible access-level modifiers specified}} expected-note{{previous modifier specified here}}
open open class OpenIsNotCompatibleWithOpen {} // expected-error {{duplicate modifier}} expected-note{{modifier already specified here}}

open typealias OpenIsNotAllowedOnTypeAliases = Int // expected-error {{only classes and overridable class members can be declared 'open'; use 'public'}}
open struct OpenIsNotAllowedOnStructs {} // expected-error {{only classes and overridable class members can be declared 'open'; use 'public'}}
open enum OpenIsNotAllowedOnEnums_AtLeastNotYet {} // expected-error {{only classes and overridable class members can be declared 'open'; use 'public'}}

/**** Open entities are at least public. ****/

func foo(object: ExternalOpenClass) {
  object.openMethod()
  object.openProperty += 5
  object[MarkerForOpenSubscripts()] += 5
}

/**** Open classes. ****/

open class ClassesMayBeDeclaredOpen {}

class ExternalSuperClassesMustBeOpen : ExternalNonOpenClass {} // expected-error {{cannot inherit from non-open class 'ExternalNonOpenClass' outside of its defining module}}

class ExternalSuperClassesMayBeOpen : ExternalOpenClass {}

class NestedClassesOfPublicTypesAreOpen : ExternalStruct.OpenClass {}

// This one is hard to diagnose.
class NestedClassesOfInternalTypesAreNotOpen : ExternalInternalStruct.OpenClass {} // expected-error {{cannot find type 'ExternalInternalStruct' in scope}}

class NestedPublicClassesOfOpenClassesAreNotOpen : ExternalOpenClass.PublicClass {} // expected-error {{cannot inherit from non-open class 'ExternalOpenClass.PublicClass' outside of its defining module}}

open final class ClassesMayNotBeBothOpenAndFinal {} // expected-error {{class cannot be declared both 'final' and 'open'}}

public class NonOpenSuperClass {} // expected-note {{superclass is declared here}}
open class OpenClassesMustHaveOpenSuperClasses : NonOpenSuperClass {} // expected-error {{superclass 'NonOpenSuperClass' of open class must be open}}

/**** Open methods. ****/

open class AnOpenClass {
  open func openMethod() {}
  open var openVar: Int = 0
  open typealias MyInt = Int // expected-error {{only classes and overridable class members can be declared 'open'; use 'public'}}
  open subscript(_: MarkerForOpenSubscripts) -> Int {
    return 0
  }
}

internal class NonOpenClassesCanHaveOpenMembers {
  open var openVar: Int = 0;
  open func openMethod() {}
}

class SubClass : ExternalOpenClass {
  override func openMethod() {}
  override var openProperty: Int { get{return 0} set{} }
  override subscript(index: MarkerForOpenSubscripts) -> Int {
    get { return 0 }
    set {}
  }

  override func nonOpenMethod() {} // expected-error {{overriding non-open instance method outside of its defining module}}
  override var nonOpenProperty: Int { get{return 0} set{} } // expected-error {{overriding non-open property outside of its defining module}}
  override subscript(index: MarkerForNonOpenSubscripts) -> Int { // expected-error {{overriding non-open subscript outside of its defining module}}
    get { return 0 }
    set {}
  }
}

open class ValidOpenSubClass : ExternalOpenClass {
  public override func openMethod() {}
  public override var openProperty: Int { get{return 0} set{} }
  public override subscript(index: MarkerForOpenSubscripts) -> Int {
    get { return 0 }
    set {}
  }
}

open class InvalidOpenSubClass : ExternalOpenClass {
  internal override func openMethod() {} // expected-error {{overriding instance method must be as accessible as the declaration it overrides}} {{3-11=open}}
  internal override var openProperty: Int { get{return 0} set{} } // expected-error {{overriding property must be as accessible as the declaration it overrides}} {{3-11=open}}
  internal override subscript(index: MarkerForOpenSubscripts) -> Int { // expected-error {{overriding subscript must be as accessible as the declaration it overrides}} {{3-11=open}}
    get { return 0 }
    set {}
  }
}

open class OpenSubClassFinalMembers : ExternalOpenClass {
  final public override func openMethod() {}
  final public override var openProperty: Int { get{return 0} set{} } 
  final public override subscript(index: MarkerForOpenSubscripts) -> Int { 
    get { return 0 }
    set {}
  }
}

open class InvalidOpenSubClassFinalMembers : ExternalOpenClass {
  final internal override func openMethod() {} // expected-error {{overriding instance method must be as accessible as its enclosing type}} {{9-17=public}}
  final internal override var openProperty: Int { get{return 0} set{} } // expected-error {{overriding property must be as accessible as its enclosing type}} {{9-17=public}}
  final internal override subscript(index: MarkerForOpenSubscripts) -> Int { // expected-error {{overriding subscript must be as accessible as its enclosing type}} {{9-17=public}}
    get { return 0 }
    set {}
  }
}

public class PublicSubClass : ExternalOpenClass {
  public override func openMethod() {}
  public override var openProperty: Int { get{return 0} set{} }
  public override subscript(index: MarkerForOpenSubscripts) -> Int {
    get { return 0 }
    set {}
  }
}


// The proposal originally made these invalid, but we changed our minds.
open class OpenSuperClass {
  public func publicMethod() {}
  public var publicProperty: Int { return 0 }
  public subscript(index: MarkerForNonOpenSubscripts) -> Int { return 0 }
}
open class OpenSubClass : OpenSuperClass {
  open override func publicMethod() {}
  open override var publicProperty: Int { return 0 }
  open override subscript(index: MarkerForNonOpenSubscripts) -> Int { return 0 }
  
}

class InvalidOpenExtensionClass { }

open extension InvalidOpenExtensionClass {  // expected-error {{extensions cannot be declared 'open'; declare individual members as 'open' instead}} {{1-6=}} {{+1:3-3=public }} {{+3:3-3=public }}
  func C() { } // Insert public
  private func A() { } // OK
  var F: Int { 3 } // Insert public
  private var G: Int { 3 } // Okay
}
