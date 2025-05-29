// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Class -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/class.h

// RUN: %check-interop-cxx-header-in-clang(%t/class.h)

public class SwiftNode {
}

public struct SwiftLinkedList {
  public var head: SwiftNode
}
