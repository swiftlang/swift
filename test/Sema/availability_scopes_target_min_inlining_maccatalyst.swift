// This test is the same as availability_refinement_contexts_target_min_inlining.swift
// but with a different run invocation to specifically test macCatalyst because
// currently there are no bots that run the tests with macCatalyst as the
// target OS.

// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -target %target-cpu-apple-ios14.4-macabi -typecheck -dump-availability-scopes -target-min-inlining-version min %s > %t.dump 2>&1
// RUN: %FileCheck --strict-whitespace %s < %t.dump

// REQUIRES: OS=macosx || OS=maccatalyst

// Verify that -target-min-inlining-version min implies 13.1 on macCatalyst.

// CHECK: {{^}}(root version=13.1
// CHECK-NEXT: {{^}}  (decl_implicit version=14.4 decl=foo()
func foo() {}
