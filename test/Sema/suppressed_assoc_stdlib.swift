// RUN: %target-typecheck-verify-swift -parse-stdlib -module-name Swift -enable-experimental-feature NoncopyableGenerics

public protocol Hello {
  associatedtype Req: ~Copyable
}
