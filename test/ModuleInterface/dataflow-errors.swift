// RUN: rm -f %t
// RUN: not %target-swift-frontend -emit-module-interface-path %t -emit-module -o /dev/null %s
// RUN: test ! -f %t
// RUN: %target-swift-frontend -emit-module-interface-path %t -typecheck %s
// RUN: test -f %t

public struct BadInit {
  public var x: Int
  public init() {
    return // without initializing 'x'
  }
}