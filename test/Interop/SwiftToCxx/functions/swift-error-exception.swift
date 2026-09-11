// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name ErrorException -clang-header-expose-decls=all-public -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/error.h
// RUN: %check-interop-cxx-header-in-clang(%t/error.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -Wno-unused-function)
// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX

public enum SimpleError: Error { case failure }

public struct DescriptiveError: Error, CustomStringConvertible {
  public let code: Int
  public var description: String { "custom error: café ☕" }
}

public func failSimply() throws { throw SimpleError.failure }
public func failDescriptively() throws { throw DescriptiveError(code: 7) }
