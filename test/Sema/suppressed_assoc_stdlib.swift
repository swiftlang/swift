// RUN: %target-typecheck-verify-swift -parse-stdlib -module-name Swift

public protocol Hello {
  associatedtype Req: ~Copyable
}
