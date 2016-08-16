// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -I %t -emit-module -emit-module-path=%t/resilient_struct.swiftmodule -module-name resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -I %t -emit-module -emit-module-path=%t/resilient_class.swiftmodule -module-name resilient_class %S/../Inputs/resilient_class.swift
// RUN: %target-swift-frontend -emit-silgen -parse-as-library -I %t %s | %FileCheck %s

import resilient_class

public class Parent {
  public final var finalProperty: String {
    return "Parent.finalProperty"
  }

  public var property: String {
    return "Parent.property"
  }

  public final class var finalClassProperty: String {
    return "Parent.finalProperty"
  }

  public class var classProperty: String {
    return "Parent.property"
  }

  public func methodOnlyInParent() {}
  public final func finalMethodOnlyInParent() {}
  public func method() {}

  public final class func finalClassMethodOnlyInParent() {}
  public class func classMethod() {}
}

public class Child : Parent {
  // CHECK-LABEL: sil @_TFC5super5Childg8propertySS
  // CHECK:         [[CASTED_SELF:%[0-9]+]] = upcast %0 : $Child to $Parent
  // CHECK:         [[SUPER_METHOD:%[0-9]+]] = function_ref @_TFC5super6Parentg8propertySS : $@convention(method) (@guaranteed Parent) -> @owned String
  public override var property: String {
    return super.property
  }

  // CHECK-LABEL: sil @_TFC5super5Childg13otherPropertySS
  // CHECK:         [[CASTED_SELF:%[0-9]+]] = upcast %0 : $Child to $Parent
  // CHECK:         [[SUPER_METHOD:%[0-9]+]] = function_ref @_TFC5super6Parentg13finalPropertySS
  public var otherProperty: String {
    return super.finalProperty
  }
}

public class Grandchild : Child {
  // CHECK-LABEL: sil @_TFC5super10Grandchild16onlyInGrandchildfT_T_
  public func onlyInGrandchild() {
    // CHECK: function_ref @_TFC5super6Parent18methodOnlyInParentfT_T_ : $@convention(method) (@guaranteed Parent) -> ()
    super.methodOnlyInParent()
    // CHECK: function_ref @_TFC5super6Parent23finalMethodOnlyInParentfT_T_
    super.finalMethodOnlyInParent()
  }

  // CHECK-LABEL: sil @_TFC5super10Grandchild6methodfT_T_
  public override func method() {
    // CHECK: function_ref @_TFC5super6Parent6methodfT_T_ : $@convention(method) (@guaranteed Parent) -> ()
    super.method()
  }
}

public class GreatGrandchild : Grandchild {
  // CHECK-LABEL: sil @_TFC5super15GreatGrandchild6methodfT_T_
  public override func method() {
    // CHECK: function_ref @_TFC5super10Grandchild6methodfT_T_ : $@convention(method) (@guaranteed Grandchild) -> ()
    super.method()
  }
}

public class ChildToResilientParent : ResilientOutsideParent {
  public override func method() {
    super.method()
  }

  public override class func classMethod() {
    super.classMethod()
  }
}

public class ChildToFixedParent : OutsideParent {
  public override func method() {
    super.method()
  }

  public override class func classMethod() {
    super.classMethod()
  }
}

public extension ResilientOutsideChild {
  public func callSuperMethod() {
    super.method()
  }

  public class func callSuperClassMethod() {
    super.classMethod()
  }
}
