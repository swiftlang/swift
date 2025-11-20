// RUN: %target-typecheck-verify-swift -enable-upcoming-feature ImplicitOpenExistentials
// RUN: %target-typecheck-verify-swift -swift-version 6

// REQUIRES: swift_feature_ImplicitOpenExistentials

#if _runtime(_ObjC)
@objc
protocol X {}

func foo<T: X>(_ val: T.Type) {}

func bar(_ val: X.Type) {
  // Only succeeds when we're allowed to open an @objc existential.
  foo(val)
}
#endif

func takeError<E: Error>(_ error: E) { }

func passError(error: any Error) {
  takeError(error)  // okay
}
