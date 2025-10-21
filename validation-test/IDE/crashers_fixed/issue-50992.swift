// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

// https://github.com/apple/swift/issues/50992

public func headers() -> [AnyHashable: String]? { fatalError() }
public var httpAdditionalHeaders: [AnyHashable : Any]?

if let userAgentHeaders = headers() {
  httpAdditionalHeaders = userAgentHeaders #^COMPLETE^#
}
