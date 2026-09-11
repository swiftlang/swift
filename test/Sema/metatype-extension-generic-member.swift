// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

// A metatype-extension member is reachable on a concrete interface metatype but
// not on a generic one: the member lives on `(any P).Type` and a generic
// parameter's metatype is not existential. The generic access must explain that
// a metatype-extension member is not inherited by a conforming type.

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IWidget { }

extension IWidget.Protocol {
  var tag: Int { 0 }
}

// Concrete access is fine.
func concrete() {
  _ = IWidget.tag
}

// Generic access is diagnosed cleanly.
func generic<Interface: IWidget>(_: Interface.Type) {
  _ = Interface.tag
  // expected-error@-1{{metatype extension member 'tag' cannot be used on conforming type 'Interface'; it is only available on the protocol metatype}}
}
