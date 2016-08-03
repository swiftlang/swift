// RUN: %target-parse-verify-swift

func foo(a :  UnsafePointer<Void>)->UnsafeMutablePointer<Void> {
  return UnsafeMutablePointer(a) // expected-error {{cannot invoke initializer for type 'UnsafeMutablePointer<_>' with an argument list of type '(UnsafePointer<Void>)'}} // expected-note {{do you want to add 'mutating'}}{{31-31=mutating: }} // expected-note {{overloads for 'UnsafeMutablePointer<_>' exist with these partially matching parameter lists: (RawPointer), (OpaquePointer), (OpaquePointer?), (UnsafeMutablePointer<Pointee>), (UnsafeMutablePointer<Pointee>?)}}
}