// RUN: %target-typecheck-verify-swift %s -enable-experimental-feature Embedded -wmo -enable-experimental-feature CustomAvailability -define-enabled-availability-domain Unicode -DBAD

// RUN: %target-swift-emit-silgen %s -enable-experimental-feature Embedded -wmo -enable-experimental-feature CustomAvailability -define-always-enabled-availability-domain Unicode | %FileCheck -check-prefix ENABLED %s

// RUN: %target-swift-emit-silgen %s -enable-experimental-feature Embedded -wmo -enable-experimental-feature CustomAvailability -define-disabled-availability-domain Unicode | %FileCheck -check-prefix DISABLED %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_CustomAvailability
// REQUIRES: swift_feature_Embedded

public func sameValue<T: Comparable>(_ x: T, _ y: T) -> Bool { x == y }

// Ensure that the symbol is suppressed when the domain is disabled.
// ENABLED: sil [ossa] @$e4main14selfComparisonySbSSF
// DISABLED-NOT: sil [ossa] @$e4main14selfComparisonySbSSF

@available(Unicode)
@export(interface)
public func selfComparison(_ string: String) -> Bool {
  sameValue(string, string)
}

#if BAD
// expected-note@+1{{add '@available' attribute to enclosing global function}}
public func badSelfComparison(_ string: String) -> Bool {
  sameValue(string, string) // expected-error{{conformance of 'String' to 'Comparable' is only available in Unicode}}
  // expected-note@-1{{add 'if #available' version check}}
}
#endif
