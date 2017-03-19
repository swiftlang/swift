// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -I %t -emit-module -emit-module-path=%t/resilient_struct.swiftmodule -module-name resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -I %t -emit-module -emit-module-path=%t/resilient_class.swiftmodule -module-name resilient_class %S/../Inputs/resilient_class.swift
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen -parse-as-library -I %t %s | %FileCheck %s

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
  // CHECK-LABEL: sil @_T05super5ChildC8propertySSfg : $@convention(method) (@guaranteed Child) -> @owned String {
  // CHECK:       bb0([[SELF:%.*]] : $Child):
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[CASTED_SELF_COPY:%[0-9]+]] = upcast [[SELF_COPY]] : $Child to $Parent
  // CHECK:         [[SUPER_METHOD:%[0-9]+]] = function_ref @_T05super6ParentC8propertySSfg : $@convention(method) (@guaranteed Parent) -> @owned String
  // CHECK:         [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:         [[RESULT:%.*]] = apply [[SUPER_METHOD]]([[BORROWED_CASTED_SELF_COPY]])
  // CHECK:         end_borrow [[BORROWED_CASTED_SELF_COPY]] from [[CASTED_SELF_COPY]]
  // ==> SEMANTIC SIL TODO: This upcast is incorrect.
  // CHECK:         destroy_value [[SELF_COPY]]
  // CHECK:         return [[RESULT]]
  public override var property: String {
    return super.property
  }

  // CHECK-LABEL: sil @_T05super5ChildC13otherPropertySSfg : $@convention(method) (@guaranteed Child) -> @owned String {
  // CHECK:       bb0([[SELF:%.*]] : $Child):
  // CHECK:         [[COPIED_SELF:%.*]] = copy_value [[SELF]]
  // CHECK:         [[CASTED_SELF_COPY:%[0-9]+]] = upcast [[COPIED_SELF]] : $Child to $Parent
  // CHECK:         [[SUPER_METHOD:%[0-9]+]] = function_ref @_T05super6ParentC13finalPropertySSfg
  // CHECK:         [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:         [[RESULT:%.*]] = apply [[SUPER_METHOD]]([[BORROWED_CASTED_SELF_COPY]])
  // CHECK:         end_borrow [[BORROWED_CASTED_SELF_COPY]] from [[CASTED_SELF_COPY]]
  // ==> SEMANTIC SIL TODO: This should be on CASTED_SELF_COPY
  // CHECK:         destroy_value [[COPIED_SELF]]
  // CHECK:         return [[RESULT]]
  public var otherProperty: String {
    return super.finalProperty
  }
}

public class Grandchild : Child {
  // CHECK-LABEL: sil @_T05super10GrandchildC06onlyInB0yyF
  public func onlyInGrandchild() {
    // CHECK: function_ref @_T05super6ParentC012methodOnlyInB0yyF : $@convention(method) (@guaranteed Parent) -> ()
    super.methodOnlyInParent()
    // CHECK: function_ref @_T05super6ParentC017finalMethodOnlyInB0yyF
    super.finalMethodOnlyInParent()
  }

  // CHECK-LABEL: sil @_T05super10GrandchildC6methodyyF
  public override func method() {
    // CHECK: function_ref @_T05super6ParentC6methodyyF : $@convention(method) (@guaranteed Parent) -> ()
    super.method()
  }
}

public class GreatGrandchild : Grandchild {
  // CHECK-LABEL: sil @_T05super15GreatGrandchildC6methodyyF
  public override func method() {
    // CHECK: function_ref @_T05super10GrandchildC6methodyyF : $@convention(method) (@guaranteed Grandchild) -> ()
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

public class GenericBase<T> {
  public func method() {}
}

public class GenericDerived<T> : GenericBase<T> {
  public override func method() {
    // CHECK-LABEL: sil shared @_T05super14GenericDerivedC6methodyyFyycfU_ : $@convention(thin) <T> (@owned GenericDerived<T>) -> ()
    // CHECK: upcast {{.*}} : $GenericDerived<T> to $GenericBase<T>
    // CHECK: return
    {
      super.method()
    }()

    // CHECK-LABEL: sil shared @_T05super14GenericDerivedC6methodyyF13localFunctionL_yylF : $@convention(thin) <T> (@owned GenericDerived<T>) -> ()
    // CHECK: upcast {{.*}} : $GenericDerived<T> to $GenericBase<T>
    // CHECK: return

    func localFunction() {
      super.method()
    }
    localFunction()

    // CHECK-LABEL: sil shared @_T05super14GenericDerivedC6methodyyF15genericFunctionL_yqd__r__lF : $@convention(thin) <T><U> (@in U, @owned GenericDerived<T>) -> ()
    // CHECK: upcast {{.*}} : $GenericDerived<T> to $GenericBase<T>
    // CHECK: return
    func genericFunction<U>(_: U) {
      super.method()
    }
    genericFunction(0)
  }
}
