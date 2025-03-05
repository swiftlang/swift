// RUN: %target-swift-frontend -dump-ast %s | %FileCheck %s

func doSomething<T>(_ t: T) {}

// CHECK: func_decl{{.*}}"outerGeneric(t:x:)" "<T>" interface_type="<T> (t: T, x: AnyObject) -> ()"

func outerGeneric<T>(t: T, x: AnyObject) {
  // Simple case -- closure captures outer generic parameter
  // CHECK: closure_expr type="() -> ()" {{.*}} discriminator=0 nonisolated captures=(<generic> t<direct> type=T, type=τ_0_0) escaping single_expression
  _ = { doSomething(t) }

  // Special case -- closure does not capture outer generic parameters
  // CHECK: closure_expr type="() -> ()" {{.*}} discriminator=1 nonisolated captures=(x<direct>) escaping single_expression
  _ = { doSomething(x) }

  // Special case -- closure captures outer generic parameter, but it does not
  // appear as the type of any expression
  // CHECK: closure_expr type="() -> ()" {{.*}} discriminator=2 nonisolated captures=(<generic> x<direct> type=T)
  _ = { if x is T {} }

  // Nested generic functions always capture outer generic parameters, even if
  // they're not mentioned in the function body
  // CHECK: func_decl{{.*}}"innerGeneric(u:)" "<U>" interface_type="<T, U> (u: U) -> ()" {{.*}} captures=(<generic> type=τ_1_0)
  func innerGeneric<U>(u: U) {}

  // Make sure we look through typealiases
  typealias TT = (a: T, b: T)

  // CHECK: func_decl{{.*}}"localFunction(tt:)" interface_type="<T> (tt: TT) -> ()" {{.*}} captures=(<generic>  type=τ_0_0)
  func localFunction(tt: TT) {}

  // CHECK: closure_expr type="(TT) -> ()" {{.*}} captures=(<generic> type=T)
  let _: (TT) -> () = { _ in }
}
