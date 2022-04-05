// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

import StdlibUnittest

func checkStringOverloadCompilationDiagnostics() {

  _ = String(cString: "string") // expected-warning {{'init(cString:)' is deprecated: Operate directly on the String}}

  _ = String(validatingUTF8: "string") // expected-warning {{init(validatingUTF8:)' is deprecated: Operate directly on the String}}

  _ = String.decodeCString("string", as: Unicode.UTF8.self) // expected-warning {{'decodeCString(_:as:repairingInvalidCodeUnits:)' is deprecated: Operate directly on the String}}

  _ = String(decodingCString: "string", as: Unicode.UTF8.self) // expected-warning {{'init(decodingCString:as:)' is deprecated: Operate directly on the String}}
}

func checkInoutConversionOverloadCompilationDiagnostics() {

  var i = UInt8.zero

  _ = String(cString: &i) // expected-warning {{'init(cString:)' is deprecated}}

  var c = CChar.zero

  _ = String(cString: &c) // expected-warning {{'init(cString:)' is deprecated}}

  _ = String(validatingUTF8: &c) // expected-warning {{init(validatingUTF8:)' is deprecated}}

  var u = Unicode.UTF8.CodeUnit.zero

  _ = String.decodeCString(&u, as: Unicode.UTF8.self) // expected-warning {{'decodeCString(_:as:repairingInvalidCodeUnits:)' is deprecated}}

  _ = String(decodingCString: &u, as: Unicode.UTF8.self) // expected-warning {{'init(decodingCString:as:)' is deprecated}}
}
