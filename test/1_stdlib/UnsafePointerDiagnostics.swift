// RUN: %target-parse-verify-swift

// Test availability attributes on UnsafePointer initializers.
// Assume the original source contains no UnsafeRawPointer types.
func unsafePointerConversionAvailability(
  mrp: UnsafeMutableRawPointer,
  rp: UnsafeRawPointer,
  umpv: UnsafeMutablePointer<Void>, // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  upv: UnsafePointer<Void>, // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
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

  // UnsafeMutablePointer<Void> to UnsafeMutableRawPointer(umpv)
  _ = UnsafeMutablePointer<Void>(rp) // expected-warning 3 {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}} expected-error {{cannot invoke initializer for type 'UnsafeMutablePointer<Void>' with an argument list of type '(UnsafeRawPointer)'}} expected-note {{overloads for 'UnsafeMutablePointer<Void>' exist with these partially matching parameter lists: (RawPointer), (OpaquePointer), (OpaquePointer?), (UnsafeMutablePointer<Pointee>), (UnsafeMutablePointer<Pointee>?)}} expected-note{{Pointer conversion restricted: use '.assumingMemoryBound(to:)' or '.bindMemory(to:capacity:)' to view memory as a type}}
  _ = UnsafeMutablePointer<Void>(mrp) // expected-error {{cannot invoke initializer for type 'UnsafeMutablePointer<Void>' with an argument list of type '(UnsafeMutableRawPointer)'}} expected-note {{}} expected-warning 3 {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}} expected-note{{overloads for 'UnsafeMutablePointer<Void>' exist with these partially matching parameter lists: (RawPointer), (OpaquePointer), (OpaquePointer?), (UnsafeMutablePointer<Pointee>), (UnsafeMutablePointer<Pointee>?)}}
  _ = UnsafeMutablePointer<Void>(umpv) // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  _ = UnsafeMutablePointer<Void>(upv)  // expected-error {{'init' has been renamed to 'init(mutating:)'}} expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  _ = UnsafeMutablePointer<Void>(umpi) // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  _ = UnsafeMutablePointer<Void>(upi)  // expected-error {{'init' has been renamed to 'init(mutating:)'}} expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  _ = UnsafeMutablePointer<Void>(umps) // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  _ = UnsafeMutablePointer<Void>(ups)  // expected-error {{'init' has been renamed to 'init(mutating:)'}} expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}

  // UnsafePointer<Void> to UnsafeRawPointer(umpv)
  _ = UnsafePointer<Void>(rp)  // expected-error {{cannot invoke initializer for type 'UnsafePointer<Void>' with an argument list of type '(UnsafeRawPointer)'}} expected-note {{}} expected-warning 3 {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}} expected-note{{overloads for 'UnsafePointer<Void>' exist with these partially matching parameter lists: (RawPointer), (OpaquePointer), (OpaquePointer?), (UnsafePointer<Pointee>), (UnsafePointer<Pointee>?), (UnsafeMutablePointer<Pointee>), (UnsafeMutablePointer<Pointee>?)}}
  _ = UnsafePointer<Void>(mrp) // expected-error {{cannot invoke initializer for type 'UnsafePointer<Void>' with an argument list of type '(UnsafeMutableRawPointer)'}} expected-note {{}} expected-warning 3 {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}} expected-note{{overloads for 'UnsafePointer<Void>' exist with these partially matching parameter lists: (RawPointer), (OpaquePointer), (OpaquePointer?), (UnsafePointer<Pointee>), (UnsafePointer<Pointee>?), (UnsafeMutablePointer<Pointee>), (UnsafeMutablePointer<Pointee>?)}}
  _ = UnsafePointer<Void>(umpv) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(upv) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(umpi) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(upi) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(umps) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(ups) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}

  _ = UnsafeMutablePointer<Int>(rp) // expected-error {{cannot invoke initializer for type 'UnsafeMutablePointer<Int>' with an argument list of type '(UnsafeRawPointer)'}} expected-note {{Pointer conversion restricted: use '.assumingMemoryBound(to:)' or '.bindMemory(to:capacity:)' to view memory as a type.}} expected-note {{}}
  _ = UnsafeMutablePointer<Int>(mrp) // expected-error {{cannot invoke initializer for type 'UnsafeMutablePointer<Int>' with an argument list of type '(UnsafeMutableRawPointer)'}} expected-note {{Pointer conversion restricted: use '.assumingMemoryBound(to:)' or '.bindMemory(to:capacity:)' to view memory as a type.}} expected-note {{}}
  _ = UnsafeMutablePointer<Int>(umpv) // expected-error {{'init' is unavailable: use 'withMemoryRebound(to:capacity:_)' to temporarily view memory as another layout-compatible type.}}
  _ = UnsafeMutablePointer<Int>(upv)  // expected-error {{'init' is unavailable: use 'withMemoryRebound(to:capacity:_)' to temporarily view memory as another layout-compatible type.}}
  _ = UnsafeMutablePointer<Int>(umpi)
  _ = UnsafeMutablePointer<Int>(upi)  // expected-error {{'init' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutablePointer<Int>(umps) // expected-error {{'init' is unavailable: use 'withMemoryRebound(to:capacity:_)' to temporarily view memory as another layout-compatible type.}}
  _ = UnsafeMutablePointer<Int>(ups)  // expected-error {{'init' is unavailable: use 'withMemoryRebound(to:capacity:_)' to temporarily view memory as another layout-compatible type.}}

  _ = UnsafePointer<Int>(rp)  // expected-error {{cannot invoke initializer for type 'UnsafePointer<Int>' with an argument list of type '(UnsafeRawPointer)'}} expected-note {{Pointer conversion restricted: use '.assumingMemoryBound(to:)' or '.bindMemory(to:capacity:)' to view memory as a type.}} expected-note {{}}
  _ = UnsafePointer<Int>(mrp) // expected-error {{cannot invoke initializer for type 'UnsafePointer<Int>' with an argument list of type '(UnsafeMutableRawPointer)'}} expected-note {{Pointer conversion restricted: use '.assumingMemoryBound(to:)' or '.bindMemory(to:capacity:)' to view memory as a type.}} expected-note {{}}
  _ = UnsafePointer<Int>(umpv) // expected-error {{'init' is unavailable: use 'withMemoryRebound(to:capacity:_)' to temporarily view memory as another layout-compatible type.}}
  _ = UnsafePointer<Int>(upv)  // expected-error {{'init' is unavailable: use 'withMemoryRebound(to:capacity:_)' to temporarily view memory as another layout-compatible type.}}
  _ = UnsafePointer<Int>(umpi)
  _ = UnsafePointer<Int>(upi)
  _ = UnsafePointer<Int>(umps) // expected-error {{'init' is unavailable: use 'withMemoryRebound(to:capacity:_)' to temporarily view memory as another layout-compatible type.}}
  _ = UnsafePointer<Int>(ups)  // expected-error {{'init' is unavailable: use 'withMemoryRebound(to:capacity:_)' to temporarily view memory as another layout-compatible type.}}
}
