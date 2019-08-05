// RUN: %target-swift-frontend -swift-version 5 -typecheck %s 2>&1 | %FileCheck %s

extension DefaultStringInterpolation {
    @available(*, deprecated) func appendInterpolation(deprecated: Int) {}
}

// Make sure diagnostics emitted via string interpolations have a reasonable source location

_ = "\(deprecated: 42)"
// CHECK: [[@LINE-1]]:7: warning: 'appendInterpolation(deprecated:)' is deprecated

_ = "hello, world\(deprecated: 42)!!!"
// CHECK: [[@LINE-1]]:19: warning: 'appendInterpolation(deprecated:)' is deprecated

_ = "\(42)\(deprecated: 42)test\(deprecated: 42)"
// CHECK: [[@LINE-1]]:12: warning: 'appendInterpolation(deprecated:)' is deprecated
// CHECK: [[@LINE-2]]:33: warning: 'appendInterpolation(deprecated:)' is deprecated

_ = """
This is a multiline literal with a deprecated interpolation:

\(deprecated: 42)
"""
// CHECK: [[@LINE-2]]:2: warning: 'appendInterpolation(deprecated:)' is deprecated
