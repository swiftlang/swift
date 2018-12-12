// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s -enable-testing
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s -O
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s -enable-testing -O

// TODO: These tests are disabled because the primal value struct makes the TBDGen be different before/after SILGen.
// UN: %empty-directory(%t)
// UN: %target-swift-frontend -typecheck -parse-as-library -module-name test %s -emit-tbd -emit-tbd-path %t/typecheck.tbd
// UN: %target-swift-frontend -emit-ir -parse-as-library -module-name test %s -emit-tbd -emit-tbd-path %t/emit-ir.tbd
// UN: diff -u %t/typecheck.tbd %t/emit-ir.tbd

@differentiable(reverse) public func publicDiffable(_ x: Float, _ y: Float) -> Float { return x }
@differentiable(reverse, wrt: (.0)) public func publicDiffableWRT(_ x: Float, _ y: Float) -> Float { return x }

@differentiable(reverse) internal func internalDiffable(_ x: Float, _ y: Float) -> Float { return x }
@differentiable(reverse, wrt: (.0)) internal func internalDiffableWRT(_ x: Float, _ y: Float) -> Float { return x }

@differentiable(reverse) private func privateDiffable(_ x: Float, _ y: Float) -> Float { return x }
@differentiable(reverse, wrt: (.0)) private func privateDiffableWRT(_ x: Float, _ y: Float) -> Float { return x }
