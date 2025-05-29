// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s         -swift-version 5 -emit-module -DM -module-name M -emit-module-path %t/M.swiftmodule -enable-upcoming-feature ExistentialAny
// RUN: %target-swift-frontend %s -verify -swift-version 5 -typecheck -I %t

// REQUIRES: swift_feature_ExistentialAny

// Test that the feature state used to compile a module is not reflected in
// the module file. The feature must not be enforced on protocols originating in
// a different module just because that module was compiled with the feature
// enabled.

#if M
public protocol P {}
#else
import M
func test(_: P) {}
#endif
