// -define-availability definitions and their uses, checked with ASTGen.
// The diagnostics for malformed definitions differ in wording from the C++
// parser's, so Availability/availability_define_parsing.swift only runs with
// the C++ parser.

// RUN: not %target-swift-frontend -typecheck %s -diagnostic-style llvm \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   -define-availability "_refuseWildcard:iOS 13.0, *, macOS 11.0" \
// RUN:   -define-availability "_justAName" \
// RUN:   -define-availability "_duplicateVersion 1.0:iOS 13.0" \
// RUN:   -define-availability "_duplicateVersion 1.0:iOS 13.0" \
// RUN:   -define-availability "_myProject 1.0:macOS 51.0" \
// RUN:   2>&1 | %FileCheck %s

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: OS=macosx

// CHECK-DAG: -define-availability argument:1:27: error: future platforms identified by '*' cannot be used in an availability macro definition
// CHECK-DAG: -define-availability argument:1:11: error: {{.*}}availability macro definition
// CHECK-DAG: -define-availability argument:1:1: error: duplicate definition of availability macro '_duplicateVersion' for version '1.0'

@available(_myProject 1.0, *)
public func good() {}

@available(_myProject 3.0, *)
public func badVersion() {}
// CHECK: availability_macro_definition.swift:[[@LINE-2]]:{{[0-9]+}}: error: reference to undefined version '3.0' for availability macro '_myProject'

@available(_myProject, *)
public func noVersion() {}
// CHECK: availability_macro_definition.swift:[[@LINE-2]]:{{[0-9]+}}: error: reference to undefined version '0' for availability macro '_myProject'
