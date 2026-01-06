// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -o %t -module-name Library %s -DLIBRARY \
// RUN:  -enable-experimental-feature CustomAvailability \
// RUN:  -define-enabled-availability-domain EnabledDomain \
// RUN:  -define-always-enabled-availability-domain AlwaysEnabledDomain \
// RUN:  -define-disabled-availability-domain DisabledDomain \
// RUN:  -define-dynamic-availability-domain DynamicDomain

// RUN: %target-typecheck-verify-swift -I %t -enable-experimental-feature CustomAvailability \
// RUN:  -define-enabled-availability-domain EnabledDomain \
// RUN:  -define-always-enabled-availability-domain AlwaysEnabledDomain \
// RUN:  -define-disabled-availability-domain DisabledDomain \
// RUN:  -define-dynamic-availability-domain DynamicDomain

// RUN: not %target-swift-frontend -typecheck -I %t -enable-experimental-feature CustomAvailability %s > %t/missing.log 2>&1
// RUN: %FileCheck %s < %t/missing.log

// REQUIRES: swift_feature_CustomAvailability

#if LIBRARY

@available(EnabledDomain)
public func availableInEnabledDomain() { }

@available(AlwaysEnabledDomain)
public func availableInAlwaysEnabledDomain() { }

#else
import Library

func test1() { // expected-note{{add '@available' attribute to enclosing global function}}
  availableInEnabledDomain() // expected-error{{'availableInEnabledDomain()' is only available in EnabledDomain}}
  // expected-note@-1{{add 'if #available' version check}}
  availableInAlwaysEnabledDomain()
}

@available(EnabledDomain)
func test2() {
  availableInEnabledDomain()
  availableInAlwaysEnabledDomain()
}

#endif

// CHECK: error: unrecognized platform name 'EnabledDomain'
// CHECK: warning: ignoring unresolved custom availability domain 'EnabledDomain'
// CHECK: warning: ignoring unresolved custom availability domain 'AlwaysEnabledDomain'
