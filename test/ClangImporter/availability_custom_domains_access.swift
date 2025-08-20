// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify \
// RUN:   -import-objc-header %S/Inputs/availability_domains_bridging_header.h \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   %s

// REQUIRES: swift_feature_CustomAvailability

private import Rivers // also re-exported by Oceans
internal import Oceans
// expected-note@-1 {{availability domain 'Arctic' imported as 'internal' from 'Oceans' here}}
// expected-note@-2 {{availability domain 'Colorado' imported as 'internal' from 'Oceans' here}}
// expected-note@-3 {{availability domain 'Grand' imported as 'internal' from 'Oceans' here}}
public import Seas

@inlinable public func inlinableFunc() {
  if #available(Colorado) { } // expected-error {{availability domain 'Colorado' is internal and cannot be referenced from an '@inlinable' function}}
  if #available(Grand) { } // expected-error {{availability domain 'Grand' is internal and cannot be referenced from an '@inlinable' function}}
  if #available(Arctic) { } // expected-error {{availability domain 'Arctic' is internal and cannot be referenced from an '@inlinable' function}}
  if #available(Baltic) { }
  if #available(BayBridge) { }
}

public func nonInlinablePublicFunc() {
  if #available(Colorado) { }
  if #available(Grand) { } // expected-warning {{availability domain 'Grand' is deprecated: Use Colorado instead}}
  if #available(Arctic) { }
  if #available(Baltic) { }
  if #available(BayBridge) { }
}
