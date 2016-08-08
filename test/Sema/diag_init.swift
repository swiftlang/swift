// RUN: %target-parse-verify-swift

func foo(a :  UnsafePointer<Void>)->UnsafeMutablePointer<Void> { // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}} // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  return UnsafeMutablePointer(a) // expected-error {{'init' has been renamed to 'init(mutating:)'}}
}
