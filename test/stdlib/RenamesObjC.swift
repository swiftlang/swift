// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

func _BridgeObjectiveC<T>(x: AutoreleasingUnsafeMutablePointer<T>) {
  _ = AutoreleasingUnsafeMutablePointer<T>.Memory.self // expected-error {{'Memory' has been renamed to 'Pointee'}} {{44-50=Pointee}} {{none}}
  _ = AutoreleasingUnsafeMutablePointer<T>() // expected-error {{'init()' is unavailable: Removed in Swift 3. Please use nil literal instead.}} {{none}}
  _ = x.memory // expected-error {{'memory' has been renamed to 'pointee'}} {{9-15=pointee}} {{none}}
}
