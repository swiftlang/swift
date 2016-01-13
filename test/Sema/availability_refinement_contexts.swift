// RUN: %target-swift-frontend -parse -dump-type-refinement-contexts %s > %t.dump 2>&1
// RUN: FileCheck --strict-whitespace %s < %t.dump

// REQUIRES: OS=macosx

// CHECK: {{^}}(root versions=[10.9.0,+Inf)

// CHECK-NEXT: {{^}}  (decl versions=[10.10,+Inf) decl=SomeClass
// CHECK-NEXT: {{^}}    (decl versions=[10.11,+Inf) decl=someMethod()
// CHECK-NEXT: {{^}}      (decl versions=[10.12,+Inf) decl=someInnerFunc()
// CHECK-NEXT: {{^}}      (decl versions=[10.12,+Inf) decl=InnerClass
// CHECK-NEXT: {{^}}        (decl versions=[10.13,+Inf) decl=innerClassMethod
// CHECK-NEXT: {{^}}    (decl versions=[10.11,+Inf) decl=someStaticProperty
// CHECK-NEXT: {{^}}    (decl versions=[10.11,+Inf) decl=someComputedProperty
// CHECK-NEXT: {{^}}    (decl versions=[10.11,+Inf) decl=someOtherMethod()
@available(OSX 10.10, *)
class SomeClass {
  @available(OSX 10.11, *)
  func someMethod() {

    @available(OSX 10.12, *)
    func someInnerFunc() { }

    @available(OSX 10.12, *)
    class InnerClass {
      @available(OSX 10.13, *)
      func innerClassMethod() { }
    }
  }

  func someUnrefinedMethod() { }

  @available(OSX 10.11, *)
  static var someStaticProperty: Int = 7

  @available(OSX 10.11, *)
  var someComputedProperty: Int {
    get { }
    set(v) { }
  }

  @available(OSX 10.11, *)
  func someOtherMethod() { }
}

// CHECK-NEXT: {{^}}  (decl versions=[10.10,+Inf) decl=someFunction()
@available(OSX 10.10, *)
func someFunction() { }

// CHECK-NEXT: {{^}}  (decl versions=[10.10,+Inf) decl=SomeProtocol
// CHECK-NEXT: {{^}}    (decl versions=[10.11,+Inf) decl=protoMethod()
// CHECK-NEXT: {{^}}    (decl versions=[10.11,+Inf) decl=protoProperty
@available(OSX 10.10, *)
protocol SomeProtocol {
  @available(OSX 10.11, *)
  func protoMethod() -> Int

  @available(OSX 10.11, *)
  var protoProperty: Int { get }
}

// CHECK-NEXT: {{^}}  (decl versions=[10.10,+Inf) decl=extension.SomeClass
// CHECK-NEXT: {{^}}    (decl versions=[10.11,+Inf) decl=someExtensionFunction()
@available(OSX 10.10, *)
extension SomeClass {
  @available(OSX 10.11, *)
  func someExtensionFunction() { }
}

// CHECK-NEXT: {{^}}  (decl versions=[10.10,+Inf) decl=functionWithStmtCondition
// CHECK-NEXT: {{^}}    (condition_following_availability versions=[10.11,+Inf)
// CHECK-NEXT: {{^}}      (condition_following_availability versions=[10.12,+Inf)
// CHECK-NEXT: {{^}}    (if_then versions=[10.12,+Inf)
// CHECK-NEXT: {{^}}      (condition_following_availability versions=[10.13,+Inf)
// CHECK-NEXT: {{^}}      (if_then versions=[10.13,+Inf)
// CHECK-NEXT: {{^}}        (condition_following_availability versions=[10.14,+Inf)
// CHECK-NEXT: {{^}}        (decl versions=[10.14,+Inf) decl=funcInGuardElse()
// CHECK-NEXT: {{^}}        (guard_fallthrough versions=[10.14,+Inf)
// CHECK-NEXT: {{^}}          (condition_following_availability versions=[10.15,+Inf)
// CHECK-NEXT: {{^}}          (guard_fallthrough versions=[10.15,+Inf)
// CHECK-NEXT: {{^}}      (decl versions=[10.13,+Inf) decl=funcInInnerIfElse()
@available(OSX 10.10, *)
func functionWithStmtCondition() {
  if #available(OSX 10.11, *),
     let x = (nil as Int?),
     #available(OSX 10.12, *) {
    if #available(OSX 10.13, *) {
      guard #available(OSX 10.14, *) else {
        @available(OSX 10.14, *)
        func funcInGuardElse() { }
      }
      guard #available(OSX 10.15, *) else { }
    } else {
      @available(OSX 10.13, *)
      func funcInInnerIfElse() { }
    }
  }
}

// CHECK-NEXT: {{^}}  (decl versions=[10.10,+Inf) decl=functionWithUnnecessaryStmtCondition
// CHECK-NEXT: {{^}}    (condition_following_availability versions=[10.12,+Inf)
// CHECK-NEXT: {{^}}    (if_then versions=[10.12,+Inf)
// CHECK-NEXT: {{^}}    (condition_following_availability versions=[10.13,+Inf)
// CHECK-NEXT: {{^}}    (if_then versions=[10.13,+Inf)

@available(OSX 10.10, *)
func functionWithUnnecessaryStmtCondition() {
  // Shouldn't introduce refinement context for then branch when unnecessary
  if #available(OSX 10.10, *) {
  }

  if #available(OSX 10.9, *) {
  }

  // Nested in conjunctive statement condition
  if #available(OSX 10.12, *),
     let x = (nil as Int?),
     #available(OSX 10.11, *) {
  }

  if #available(OSX 10.13, *),
     #available(OSX 10.13, *) {
  }

  // Wildcard is same as minimum deployment target
  if #available(iOS 7.0, *) {
  }
}

// CHECK-NEXT: {{^}}  (decl versions=[10.10,+Inf) decl=functionWithUnnecessaryStmtConditionsHavingElseBranch
// CHECK-NEXT: {{^}}    (if_else versions=empty
// CHECK-NEXT: {{^}}      (decl versions=empty decl=funcInInnerIfElse()
// CHECK-NEXT: {{^}}    (if_else versions=empty
// CHECK-NEXT: {{^}}    (guard_else versions=empty
// CHECK-NEXT: {{^}}    (guard_else versions=empty
// CHECK-NEXT: {{^}}    (if_else versions=empty

@available(OSX 10.10, *)
func functionWithUnnecessaryStmtConditionsHavingElseBranch(p: Int?) {
  // Else branch context version is bottom when check is unnecessary
  if #available(OSX 10.10, *) {
  } else {
    if #available(OSX 10.11, *) {
    }

    @available(OSX 10.11, *)
    func funcInInnerIfElse() { }

    if #available(iOS 7.0, *) {
    } else {
    }
  }

  if #available(iOS 7.0, *) {
  } else {
  }

  guard #available(iOS 8.0, *) else { }

  // Else branch will execute if p is nil, so it is not dead.
  if #available(iOS 7.0, *),
     let x = p {
  } else {
  }

  if #available(iOS 7.0, *),
     let x = p,
     #available(iOS 7.0, *) {
  } else {
  }

  // Else branch is dead
  guard #available(iOS 7.0, *),
        #available(iOS 8.0, *) else { }

  if #available(OSX 10.10, *),
     #available(OSX 10.10, *) {
  } else {
  }

}

// CHECK-NEXT: {{^}}  (decl versions=[10.10,+Inf) decl=functionWithWhile()
// CHECK-NEXT: {{^}}    (condition_following_availability versions=[10.11,+Inf)
// CHECK-NEXT: {{^}}    (while_body versions=[10.11,+Inf)
// CHECK-NEXT: {{^}}      (decl versions=[10.13,+Inf) decl=funcInWhileBody()
@available(OSX 10.10, *)
func functionWithWhile() {
  while #available(OSX 10.11, *),
        let x = (nil as Int?) {
    @available(OSX 10.13, *)
    func funcInWhileBody() { }
  }
}
