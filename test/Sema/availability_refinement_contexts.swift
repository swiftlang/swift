// RUN: %target-swift-frontend -typecheck -dump-type-refinement-contexts %s > %t.dump 2>&1
// RUN: %FileCheck --strict-whitespace %s < %t.dump

// REQUIRES: OS=macosx

// CHECK: {{^}}(root versions=[10.{{[0-9]+}}.0,+Inf)

// CHECK-NEXT: {{^}}  (decl versions=[10.51,+Inf) decl=SomeClass
// CHECK-NEXT: {{^}}    (decl versions=[10.52,+Inf) decl=someMethod()
// CHECK-NEXT: {{^}}      (decl versions=[10.53,+Inf) decl=someInnerFunc()
// CHECK-NEXT: {{^}}      (decl versions=[10.53,+Inf) decl=InnerClass
// CHECK-NEXT: {{^}}        (decl versions=[10.54,+Inf) decl=innerClassMethod
// CHECK-NEXT: {{^}}    (decl versions=[10.52,+Inf) decl=someStaticProperty
// CHECK-NEXT: {{^}}    (decl versions=[10.52,+Inf) decl=someComputedProperty
// CHECK-NEXT: {{^}}    (decl versions=[10.52,+Inf) decl=someOtherMethod()
@available(OSX 10.51, *)
class SomeClass {
  @available(OSX 10.52, *)
  func someMethod() {

    @available(OSX 10.53, *)
    func someInnerFunc() { }

    @available(OSX 10.53, *)
    class InnerClass {
      @available(OSX 10.54, *)
      func innerClassMethod() { }
    }
  }
  
  func someUnrefinedMethod() { }

  @available(OSX 10.52, *)
  static var someStaticProperty: Int = 7

  @available(OSX 10.52, *)
  var someComputedProperty: Int {
    get { }
    set(v) { }
  }

  @available(OSX 10.52, *)
  func someOtherMethod() { }
}

// CHECK-NEXT: {{^}}  (decl versions=[10.51,+Inf) decl=someFunction()
@available(OSX 10.51, *)
func someFunction() { }

// CHECK-NEXT: {{^}}  (decl versions=[10.51,+Inf) decl=SomeProtocol
// CHECK-NEXT: {{^}}    (decl versions=[10.52,+Inf) decl=protoMethod()
// CHECK-NEXT: {{^}}    (decl versions=[10.52,+Inf) decl=protoProperty
@available(OSX 10.51, *)
protocol SomeProtocol {
  @available(OSX 10.52, *)
  func protoMethod() -> Int

  @available(OSX 10.52, *)
  var protoProperty: Int { get }
}

// CHECK-NEXT: {{^}}  (decl versions=[10.51,+Inf) decl=extension.SomeClass
// CHECK-NEXT: {{^}}    (decl versions=[10.52,+Inf) decl=someExtensionFunction()
@available(OSX 10.51, *)
extension SomeClass {
  @available(OSX 10.52, *)
  func someExtensionFunction() { }
}

// CHECK-NEXT: {{^}}  (decl versions=[10.51,+Inf) decl=functionWithStmtCondition
// CHECK-NEXT: {{^}}    (condition_following_availability versions=[10.52,+Inf)
// CHECK-NEXT: {{^}}      (condition_following_availability versions=[10.53,+Inf)
// CHECK-NEXT: {{^}}    (if_then versions=[10.53,+Inf)
// CHECK-NEXT: {{^}}      (condition_following_availability versions=[10.54,+Inf)
// CHECK-NEXT: {{^}}      (if_then versions=[10.54,+Inf)
// CHECK-NEXT: {{^}}        (condition_following_availability versions=[10.55,+Inf)
// CHECK-NEXT: {{^}}        (decl versions=[10.55,+Inf) decl=funcInGuardElse()
// CHECK-NEXT: {{^}}        (guard_fallthrough versions=[10.55,+Inf)
// CHECK-NEXT: {{^}}          (condition_following_availability versions=[10.56,+Inf)
// CHECK-NEXT: {{^}}          (guard_fallthrough versions=[10.56,+Inf)
// CHECK-NEXT: {{^}}      (decl versions=[10.57,+Inf) decl=funcInInnerIfElse()
@available(OSX 10.51, *)
func functionWithStmtCondition() {
  if #available(OSX 10.52, *),
     let x = (nil as Int?),
     #available(OSX 10.53, *) {
    if #available(OSX 10.54, *) {
      guard #available(OSX 10.55, *) else {
        @available(OSX 10.55, *)
        func funcInGuardElse() { }
      }
      guard #available(OSX 10.56, *) else { }
    } else {
      @available(OSX 10.57, *)
      func funcInInnerIfElse() { }
    }
  }
}

// CHECK-NEXT: {{^}}  (decl versions=[10.51,+Inf) decl=functionWithUnnecessaryStmtCondition
// CHECK-NEXT: {{^}}    (condition_following_availability versions=[10.53,+Inf)
// CHECK-NEXT: {{^}}    (if_then versions=[10.53,+Inf)
// CHECK-NEXT: {{^}}    (condition_following_availability versions=[10.54,+Inf)
// CHECK-NEXT: {{^}}    (if_then versions=[10.54,+Inf)

@available(OSX 10.51, *)
func functionWithUnnecessaryStmtCondition() {
  // Shouldn't introduce refinement context for then branch when unnecessary
  if #available(OSX 10.51, *) {
  }

  if #available(OSX 10.9, *) {
  }

  // Nested in conjunctive statement condition
  if #available(OSX 10.53, *),
     let x = (nil as Int?),
     #available(OSX 10.52, *) {
  }

  if #available(OSX 10.54, *),
     #available(OSX 10.54, *) {
  }

  // Wildcard is same as minimum deployment target
  if #available(iOS 7.0, *) {
  }
}

// CHECK-NEXT: {{^}}  (decl versions=[10.51,+Inf) decl=functionWithUnnecessaryStmtConditionsHavingElseBranch
// CHECK-NEXT: {{^}}    (if_else versions=empty
// CHECK-NEXT: {{^}}      (decl versions=empty decl=funcInInnerIfElse()
// CHECK-NEXT: {{^}}    (if_else versions=empty
// CHECK-NEXT: {{^}}    (guard_else versions=empty
// CHECK-NEXT: {{^}}    (guard_else versions=empty
// CHECK-NEXT: {{^}}    (if_else versions=empty

@available(OSX 10.51, *)
func functionWithUnnecessaryStmtConditionsHavingElseBranch(p: Int?) {
  // Else branch context version is bottom when check is unnecessary
  if #available(OSX 10.51, *) {
  } else {
    if #available(OSX 10.52, *) {
    }

    @available(OSX 10.52, *)
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

  if #available(OSX 10.51, *),
     #available(OSX 10.51, *) {
  } else {
  }

}

// CHECK-NEXT: {{^}}  (decl versions=[10.51,+Inf) decl=functionWithWhile()
// CHECK-NEXT: {{^}}    (condition_following_availability versions=[10.52,+Inf)
// CHECK-NEXT: {{^}}    (while_body versions=[10.52,+Inf)
// CHECK-NEXT: {{^}}      (decl versions=[10.54,+Inf) decl=funcInWhileBody()
@available(OSX 10.51, *)
func functionWithWhile() {
  while #available(OSX 10.52, *),
        let x = (nil as Int?) {
    @available(OSX 10.54, *)
    func funcInWhileBody() { }
  }
}
