// RUN: %target-parse-verify-swift

func foo(a :  UnsafePointer<Void>)->UnsafeMutablePointer<Void> {
  return UnsafeMutablePointer(a) // expected-error {{'init' has been renamed to 'init(mutating:)'}}
}
