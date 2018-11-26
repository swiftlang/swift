// RUN: %target-typecheck-verify-swift

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

  let omrp: UnsafeMutableRawPointer? = mrp
  let orp: UnsafeRawPointer? = rp
  let oumpv: UnsafeMutablePointer<Void> = umpv  // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  let oupv: UnsafePointer<Void>? = upv  // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  let oumpi: UnsafeMutablePointer<Int>? = umpi
  let oupi: UnsafePointer<Int>? = upi
  let oumps: UnsafeMutablePointer<String>? = umps
  let oups: UnsafePointer<String>? = ups

  _ = UnsafeMutableRawPointer(mrp)
  _ = UnsafeMutableRawPointer(rp)   // expected-error {{'init(_:)' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(umpv)
  _ = UnsafeMutableRawPointer(upv)  // expected-error {{'init(_:)' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(umpi)
  _ = UnsafeMutableRawPointer(upi)  // expected-error {{'init(_:)' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(umps)
  _ = UnsafeMutableRawPointer(ups)  // expected-error {{'init(_:)' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(omrp)
  _ = UnsafeMutableRawPointer(orp)   // expected-error {{'init(_:)' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(oumpv)
  _ = UnsafeMutableRawPointer(oupv)  // expected-error {{'init(_:)' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(oumpi)
  _ = UnsafeMutableRawPointer(oupi)  // expected-error {{'init(_:)' has been renamed to 'init(mutating:)'}}
  _ = UnsafeMutableRawPointer(oumps)
  _ = UnsafeMutableRawPointer(oups)  // expected-error {{'init(_:)' has been renamed to 'init(mutating:)'}}

  // These all correctly pass with no error.
  _ = UnsafeRawPointer(mrp)
  _ = UnsafeRawPointer(rp)
  _ = UnsafeRawPointer(umpv)
  _ = UnsafeRawPointer(upv)
  _ = UnsafeRawPointer(umpi)
  _ = UnsafeRawPointer(upi)
  _ = UnsafeRawPointer(umps)
  _ = UnsafeRawPointer(ups)
  _ = UnsafeRawPointer(omrp)
  _ = UnsafeRawPointer(orp)
  _ = UnsafeRawPointer(oumpv)
  _ = UnsafeRawPointer(oupv)
  _ = UnsafeRawPointer(oumpi)
  _ = UnsafeRawPointer(oupi)
  _ = UnsafeRawPointer(oumps)
  _ = UnsafeRawPointer(oups)
  _ = UnsafePointer<Int>(upi)
  _ = UnsafePointer<Int>(oumpi)
  _ = UnsafePointer<Int>(oupi)
  _ = UnsafeMutablePointer<Int>(umpi)
  _ = UnsafeMutablePointer<Int>(oumpi)

  _ = UnsafeMutablePointer<Void>(rp) // expected-warning 4 {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}} expected-error {{cannot convert value of type 'UnsafeRawPointer' to expected argument type 'RawPointer'}}
  _ = UnsafeMutablePointer<Void>(mrp) // expected-error {{cannot convert value of type 'UnsafeMutableRawPointer' to expected argument type 'RawPointer'}} expected-warning 4 {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  _ = UnsafeMutablePointer<Void>(umpv) // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  _ = UnsafeMutablePointer<Void>(umpi) // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}
  _ = UnsafeMutablePointer<Void>(umps) // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}

  _ = UnsafePointer<Void>(rp)  // expected-error {{cannot convert value of type 'UnsafeRawPointer' to expected argument type 'RawPointer'}} expected-warning 4 {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(mrp) // expected-error {{cannot convert value of type 'UnsafeMutableRawPointer' to expected argument type 'RawPointer'}} expected-warning 4 {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(umpv) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(upv) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(umpi) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(upi) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(umps) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}
  _ = UnsafePointer<Void>(ups) // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}

  _ = UnsafeMutablePointer<Int>(rp) // expected-error {{cannot convert value of type 'UnsafeRawPointer' to expected argument type 'RawPointer'}} 
  _ = UnsafeMutablePointer<Int>(mrp) // expected-error {{cannot convert value of type 'UnsafeMutableRawPointer' to expected argument type 'RawPointer'}} 
  _ = UnsafeMutablePointer<Int>(orp) // expected-error {{cannot convert value of type 'UnsafeRawPointer?' to expected argument type 'RawPointer'}}
  _ = UnsafeMutablePointer<Int>(omrp) // expected-error {{cannot convert value of type 'UnsafeMutableRawPointer?' to expected argument type 'RawPointer'}} 

  _ = UnsafePointer<Int>(rp)  // expected-error {{cannot convert value of type 'UnsafeRawPointer' to expected argument type 'RawPointer'}}
  _ = UnsafePointer<Int>(mrp) // expected-error {{cannot convert value of type 'UnsafeMutableRawPointer' to expected argument type 'RawPointer'}}
  _ = UnsafePointer<Int>(orp)  // expected-error {{cannot convert value of type 'UnsafeRawPointer?' to expected argument type 'RawPointer'}}
  _ = UnsafePointer<Int>(omrp) // expected-error {{cannot convert value of type 'UnsafeMutableRawPointer?' to expected argument type 'RawPointer'}}
}

func unsafeRawBufferPointerConversions(
  mrp: UnsafeMutableRawPointer,
  rp: UnsafeRawPointer,
  mrbp: UnsafeMutableRawBufferPointer,
  rbp: UnsafeRawBufferPointer,
  mbpi: UnsafeMutableBufferPointer<Int>,
  bpi: UnsafeBufferPointer<Int>) {

  let omrp: UnsafeMutableRawPointer? = mrp
  let orp: UnsafeRawPointer? = rp

  _ = UnsafeMutableRawBufferPointer(start: mrp, count: 1)
  _ = UnsafeRawBufferPointer(start: mrp, count: 1)
  _ = UnsafeMutableRawBufferPointer(start: rp, count: 1) // expected-error {{cannot convert value of type 'UnsafeRawPointer' to expected argument type 'UnsafeMutableRawPointer?'}}
  _ = UnsafeRawBufferPointer(start: rp, count: 1)
  _ = UnsafeMutableRawBufferPointer(mrbp)
  _ = UnsafeRawBufferPointer(mrbp)
  _ = UnsafeMutableRawBufferPointer(rbp) // expected-error {{cannot invoke initializer for type 'UnsafeMutableRawBufferPointer' with an argument list of type '(UnsafeRawBufferPointer)'}} expected-note {{overloads for 'UnsafeMutableRawBufferPointer' exist with these partially matching parameter lists: (UnsafeMutableRawBufferPointer), (UnsafeMutableBufferPointer<T>)}}
  _ = UnsafeRawBufferPointer(rbp)
  _ = UnsafeMutableRawBufferPointer(mbpi)
  _ = UnsafeRawBufferPointer(mbpi)
  _ = UnsafeMutableRawBufferPointer(bpi) // expected-error {{cannot invoke initializer for type 'UnsafeMutableRawBufferPointer' with an argument list of type '(UnsafeBufferPointer<Int>)'}} expected-note {{overloads for 'UnsafeMutableRawBufferPointer' exist with these partially matching parameter lists: (UnsafeMutableRawBufferPointer), (UnsafeMutableBufferPointer<T>)}}
  _ = UnsafeRawBufferPointer(bpi)
  _ = UnsafeMutableRawBufferPointer(start: omrp, count: 1)
  _ = UnsafeRawBufferPointer(start: omrp, count: 1)
  _ = UnsafeMutableRawBufferPointer(start: orp, count: 1) // expected-error {{cannot convert value of type 'UnsafeRawPointer?' to expected argument type 'UnsafeMutableRawPointer?'}}
  _ = UnsafeRawBufferPointer(start: orp, count: 1)
}