// RUN: not %target-swift-frontend %s -typecheck

// https://github.com/apple/swift/issues/71701

struct S<each T> {
  init(_ values: repeat each T) {}
}

_ = S(nil)
_ = S.init(nil)
