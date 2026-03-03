// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple

// https://github.com/swiftlang/swift/issues/74545

func Map<Source, Destination>(_ transform: @escaping (Source) -> Destination) -> any Sequence<Destination> {
  return []
}

for i in Map({$0 + 1}) {
  print(i)
}
