// This test is the same as availability_refinement_contexts_target_min_inlining.swift
// but with a different run invocation to specifically test macCatalyst because
// currently there are no bots that run the tests with macCatalyst as the
// target OS.

// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -target %target-cpu-apple-ios14.4-macabi -typecheck -dump-type-refinement-contexts -target-min-inlining-version min %s > %t.dump 2>&1
// RUN: %FileCheck --strict-whitespace %s < %t.dump

// REQUIRES: maccatalyst_support

// Verify that -target-min-inlining-version min implies 13.1 on macCatalyst.

// CHECK: {{^}}(root versions=[13.1,+Inf)
// CHECK-NEXT: {{^}}  (decl_implicit versions=[14.4.0,+Inf) decl=foo()
func foo() {}
