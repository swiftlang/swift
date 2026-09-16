// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

// A synthesized metatype-extension member (`IID`) is reachable on a concrete
// interface metatype but not on a generic one: the member lives on `(any P).Type`
// and a generic parameter's metatype is not existential.  The generic access
// must explain that a metatype-extension member is not inherited by a
// conforming type; it previously reached the constraint solver's
// inaccessible-member fallback and emitted "failed to produce diagnostic for
// expression" instead.

import COM

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IWidget: IUnknown { }

// Concrete access is fine.
func concrete() {
  _ = IWidget.IID
}

// Generic access is diagnosed cleanly.
func generic<Interface: IUnknown>(_: Interface.Type) {
  _ = Interface.IID
  // expected-error@-1 {{metatype extension member 'IID' cannot be used on conforming type 'Interface'; it is only available on the protocol metatype}}
}
