// RUN: %target-parse-verify-swift

// Test availability attributes on UnsafePointer initializers.
// Assume the original source contains no UnsafeRawPointer types.
//
// TODO:
// - implement the Unsafe[Mutable]Pointer<Void> to Unsafe[Mutable]RawPointer rename
// - test multiple fix-its per line: type rename + initializer rename/diagnostic
func unsafePointerConversionAvailability(
  mrp: UnsafeMutableRawPointer,
  rp: UnsafeRawPointer,
  umpv: UnsafeMutablePointer<Void>,
  upv: UnsafePointer<Void>,
  umpi: UnsafeMutablePointer<Int>,
  upi: UnsafePointer<Int>,
  umps: UnsafeMutablePointer<String>,
  ups: UnsafePointer<String>) {

  _ = UnsafeMutableRawPointer(mrp)
  _ = UnsafeMutableRawPointer(rp)   // expected-error {{'init' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(umpv)
  _ = UnsafeMutableRawPointer(upv)  // expected-error {{'init' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(umpi)
  _ = UnsafeMutableRawPointer(upi)  // expected-error {{'init' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(umps)
  _ = UnsafeMutableRawPointer(ups)  // expected-error {{'init' has been renamed to 'init(mutating:)'}}

  // These all correctly pass with no error.
  _ = UnsafeRawPointer(mrp)
  _ = UnsafeRawPointer(rp)
  _ = UnsafeRawPointer(umpv)
  _ = UnsafeRawPointer(upv)
  _ = UnsafeRawPointer(umpi)
  _ = UnsafeRawPointer(upi)
  _ = UnsafeRawPointer(umps)
  _ = UnsafeRawPointer(ups)

  // FIXME: All of these should yield a fix-it to rename
  // UnsafeMutablePointer<Void> to UnsafeMutableRawPointer(umpv)
  _ = UnsafeMutablePointer<Void>(rp) // expected-error {{cannot invoke initializer for type 'UnsafeMutablePointer<Void>' with an argument list of type '(UnsafeRawPointer)'}} expected-note {{}}
  _ = UnsafeMutablePointer<Void>(mrp) // expected-error {{cannot invoke initializer for type 'UnsafeMutablePointer<Void>' with an argument list of type '(UnsafeMutableRawPointer)'}} expected-note {{}}
  _ = UnsafeMutablePointer<Void>(umpv)
  _ = UnsafeMutablePointer<Void>(upv)  // expected-error {{'init' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutablePointer<Void>(umpi)
  _ = UnsafeMutablePointer<Void>(upi)  // expected-error {{'init' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutablePointer<Void>(umps)
  _ = UnsafeMutablePointer<Void>(ups)  // expected-error {{'init' has been renamed to 'init(mutating:)'}}

  // FIXME: All of these should yield a fix-it to rename
  // UnsafePointer<Void> to UnsafeRawPointer(umpv)
  _ = UnsafePointer<Void>(rp)  // expected-error {{cannot invoke initializer for type 'UnsafePointer<Void>' with an argument list of type '(UnsafeRawPointer)'}} expected-note {{}}
  _ = UnsafePointer<Void>(mrp) // expected-error {{cannot invoke initializer for type 'UnsafePointer<Void>' with an argument list of type '(UnsafeMutableRawPointer)'}} expected-note {{}}
  _ = UnsafePointer<Void>(umpv) 
  _ = UnsafePointer<Void>(upv)
  _ = UnsafePointer<Void>(umpi)
  _ = UnsafePointer<Void>(upi)
  _ = UnsafePointer<Void>(umps)
  _ = UnsafePointer<Void>(ups)

  // FIXME: Conversion from UnsafePointer<Void> or UnsafeRawPointer to a typed
  // pointer should have a diagnostic: 'init' is unavailable: Conversion
  // restricted. Use 'assumingMemoryBound(to:)' or 'bindMemory(to:capacity:)'
  _ = UnsafeMutablePointer<Int>(rp) // expected-error {{cannot invoke initializer for type 'UnsafeMutablePointer<Int>' with an argument list of type '(UnsafeRawPointer)'}} expected-note {{}}
  _ = UnsafeMutablePointer<Int>(mrp) // expected-error {{cannot invoke initializer for type 'UnsafeMutablePointer<Int>' with an argument list of type '(UnsafeMutableRawPointer)'}} expected-note {{}}
  _ = UnsafeMutablePointer<Int>(umpv) // expected-error {{'init' is unavailable: use 'withMemoryRebound(to:capacity:_)' to temporarily view memory as another layout-compatible type.}}
  _ = UnsafeMutablePointer<Int>(upv)  // expected-error {{'init' is unavailable: use 'withMemoryRebound(to:capacity:_)' to temporarily view memory as another layout-compatible type.}}
  _ = UnsafeMutablePointer<Int>(umpi)
  _ = UnsafeMutablePointer<Int>(upi)  // expected-error {{'init' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutablePointer<Int>(umps) // expected-error {{'init' is unavailable: use 'withMemoryRebound(to:capacity:_)' to temporarily view memory as another layout-compatible type.}}
  _ = UnsafeMutablePointer<Int>(ups)  // expected-error {{'init' is unavailable: use 'withMemoryRebound(to:capacity:_)' to temporarily view memory as another layout-compatible type.}}

  _ = UnsafePointer<Int>(rp)  // expected-error {{cannot invoke initializer for type 'UnsafePointer<Int>' with an argument list of type '(UnsafeRawPointer)'}} expected-note {{}}
  _ = UnsafePointer<Int>(mrp) // expected-error {{cannot invoke initializer for type 'UnsafePointer<Int>' with an argument list of type '(UnsafeMutableRawPointer)'}} expected-note {{}}
  _ = UnsafePointer<Int>(umpv) // expected-error {{'init' is unavailable: use 'withMemoryRebound(to:capacity:_)' to temporarily view memory as another layout-compatible type.}}
  _ = UnsafePointer<Int>(upv)  // expected-error {{'init' is unavailable: use 'withMemoryRebound(to:capacity:_)' to temporarily view memory as another layout-compatible type.}}
  _ = UnsafePointer<Int>(umpi)
  _ = UnsafePointer<Int>(upi)
  _ = UnsafePointer<Int>(umps) // expected-error {{'init' is unavailable: use 'withMemoryRebound(to:capacity:_)' to temporarily view memory as another layout-compatible type.}}
  _ = UnsafePointer<Int>(ups)  // expected-error {{'init' is unavailable: use 'withMemoryRebound(to:capacity:_)' to temporarily view memory as another layout-compatible type.}}
}
