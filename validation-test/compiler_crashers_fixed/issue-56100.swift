// RUN: %swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename %s

// https://github.com/apple/swift/issues/56100

extension Result {
  public init(_ value: Success?) {
    self = value.map(#^COMPLETE^#)
  }
}

public func materialize<T>(_ f: T) {}
