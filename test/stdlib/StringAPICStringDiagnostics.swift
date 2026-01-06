// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

import StdlibUnittest

func checkStringOverloadCompilationDiagnostics() {

  _ = String(cString: "string") // expected-warning {{'init(cString:)' is deprecated: Use a copy of the String argument}}{{documentation-file=deprecated-declaration}}

  _ = String(validatingUTF8: "string") // expected-warning {{init(validatingUTF8:)' is deprecated: Use a copy of the String argument}}{{documentation-file=deprecated-declaration}}

  _ = String(validatingCString: "string") // expected-warning {{'init(validatingCString:)' is deprecated: Use a copy of the String argument}}{{documentation-file=deprecated-declaration}}

  _ = String.decodeCString("string", as: Unicode.UTF8.self) // expected-warning {{'decodeCString(_:as:repairingInvalidCodeUnits:)' is deprecated: Use a copy of the String argument}}{{documentation-file=deprecated-declaration}}

  _ = String(decodingCString: "string", as: Unicode.UTF8.self) // expected-warning {{'init(decodingCString:as:)' is deprecated: Use a copy of the String argument}}{{documentation-file=deprecated-declaration}}
}

func checkInoutConversionOverloadCompilationDiagnostics() {

  var i = UInt8.zero

  _ = String(cString: &i) // expected-warning {{'init(cString:)' is deprecated: Use String(_ scalar: Unicode.Scalar)}}{{documentation-file=deprecated-declaration}}

  var c = CChar.zero

  _ = String(cString: &c) // expected-warning {{'init(cString:)' is deprecated: Use String(_ scalar: Unicode.Scalar)}}{{documentation-file=deprecated-declaration}}

  _ = String(validatingUTF8: &c) // expected-warning {{init(validatingUTF8:)' is deprecated: Use String(_ scalar: Unicode.Scalar)}}{{documentation-file=deprecated-declaration}}

  _ = String(validatingCString: &c) // expected-warning {{'init(validatingCString:)' is deprecated: Use String(_ scalar: Unicode.Scalar)}}{{documentation-file=deprecated-declaration}}

  var u = Unicode.UTF8.CodeUnit.zero

  _ = String.decodeCString(&u, as: Unicode.UTF8.self) // expected-warning {{'decodeCString(_:as:repairingInvalidCodeUnits:)' is deprecated: Use String(_ scalar: Unicode.Scalar)}}{{documentation-file=deprecated-declaration}}

  _ = String(decodingCString: &u, as: Unicode.UTF8.self) // expected-warning {{'init(decodingCString:as:)' is deprecated: Use String(_ scalar: Unicode.Scalar)}}{{documentation-file=deprecated-declaration}}
}
