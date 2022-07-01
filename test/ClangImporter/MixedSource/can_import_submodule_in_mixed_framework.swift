// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/WithSubmodule.framework %t
// RUN: %target-swift-frontend -emit-module -o %t/WithSubmodule.framework/Modules/WithSubmodule.swiftmodule/%target-swiftmodule-name %t/WithSubmodule.framework/Empty.swift -import-underlying-module -F %t -module-name WithSubmodule

// RUN: %target-typecheck-verify-swift -F %t

// Testing 'canImport()' non-existing submodule in a top module loadable by other loaders.

#if !canImport(WithSubmodule.Submodule)
#error("Should can import WithSubmodule.Submodule")
#endif

// Should fail if checked for a non-existing submodule.
#if canImport(WithSubmodule.ButNotMe)
import WithSubmodule.Submodule
#endif

func testNotImported() {
    fromSubmodule = 5
    // expected-error@-1 {{cannot find 'fromSubmodule' in scope}}
}
