// RUN: %target-build-swift %s
// RUN: %target-swift-frontend -c -g -Xllvm -verify-di-holes=true %s

// Every so often this test crashes the linker on Linux
// REQUIRES: rdar87254800

// SR-15566: Differentiable functions with control flow yield an assertion failure: "location is a VarDecl, but SILDebugVariable is empty"

import _Differentiation

public struct Test: Differentiable {
  public var v1: [[Float]]

  @differentiable(reverse)
  public init(v1: [[Float]]) {
    if v1.count != 2 {
      fatalError("Mismatched counts")
    }
    self.v1 = v1
  }
}

// Assertion failed: ((!dyn_cast_or_null<VarDecl>(Loc.getAsASTNode<Decl>()) || Var) && "location is a VarDecl, but SILDebugVariable is empty"), function createAllocStack, file SILBuilder.h, line 389.
