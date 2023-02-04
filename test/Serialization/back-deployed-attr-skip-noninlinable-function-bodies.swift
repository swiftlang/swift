// RUN: %target-swift-frontend -parse-as-library -enable-library-evolution -emit-module -module-name Test -experimental-skip-non-inlinable-function-bodies %s

@backDeployed(before: SwiftStdlib 5.7)
public func foo() {}
