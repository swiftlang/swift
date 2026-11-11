// rdar://177314255 (https://github.com/swiftlang/swift/issues/89207)
// RUN: %target-typecheck-verify-swift

func wrong<T>(_ value: T) -> (Substring, T.Index)? where T: StringProtocol {
  let index = value.endIndex
  return (value[..<index], index) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

func correct1<T>(_ value: T) -> (T.SubSequence, T.Index)? where T: StringProtocol {
  let index = value.endIndex
  return (value[..<index], index)
}

func correct2<T>(_ value: T) -> (Substring, T.Index)? where T: StringProtocol, T.SubSequence == Substring {
  let index = value.endIndex
  return (value[..<index], index)
}
