// RUN: %target-typecheck-verify-swift \
// RUN:  -enable-experimental-feature CustomAvailability \
// RUN:  -define-enabled-availability-domain EnabledDomain \
// RUN:  -define-disabled-availability-domain DisabledDomain \
// RUN:  -define-dynamic-availability-domain DynamicDomain

// REQUIRES: swift_feature_CustomAvailability

if #available(EnabledDomain) { }
if #available(DisabledDomain) { }
if #available(DynamicDomain) { }
if #available(UnknownDomain) { } // expected-error {{unrecognized platform name 'UnknownDomain'}}
// expected-error@-1 {{condition required for target platform}}

if #unavailable(EnabledDomain) { }
if #unavailable(DisabledDomain) { }
if #unavailable(DynamicDomain) { }
if #unavailable(UnknownDomain) { } // expected-error {{unrecognized platform name 'UnknownDomain'}}

if #available(EnabledDomain 1.0) { } // expected-error {{unexpected version number for EnabledDomain}}
if #available(EnabledDomain, DisabledDomain) { } // expected-error {{EnabledDomain availability must be specified alone}}

if #available(EnabledDomain, swift 5) { } // expected-error {{EnabledDomain availability must be specified alone}}

while #available(EnabledDomain) { }

guard #available(EnabledDomain) else { }
