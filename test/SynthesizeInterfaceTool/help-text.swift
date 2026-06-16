// RUN: not %swift-synthesize-interface -help 2>&1 | %FileCheck %s
// CHECK: USAGE: swift-synthesize-interface

// The help text should reflect the name the user invoked the tool by, not the
// path of the underlying swift-frontend binary the symlink resolves to.
