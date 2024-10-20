// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-module -emit-module-path %t/P.swiftmodule -parse-as-library -module-name P -DP %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -I%t -parse-as-library -module-name Q -c %s -o /dev/null -validate-tbd-against-ir=missing

// REQUIRES: concurrency

#if P
public protocol P {
  func f() async
}
#else
import P

protocol Q: P { }

extension Q {
  public func f() async { }
}

public struct S: Q {
  public init() { }
}
#endif
