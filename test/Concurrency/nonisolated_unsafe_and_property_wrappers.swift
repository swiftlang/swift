// RUN: %target-typecheck-verify-swift -language-mode 6 -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency

class Storage {
  var data: [String: Bool] = [:]
}

@propertyWrapper
struct StorageBacked {
  let key: String
  let storage: Storage
  let defaultValue: Bool

  var wrappedValue: Bool {
    get { storage.data[key] ?? defaultValue }
    set { }
  }
}

nonisolated(unsafe) private let storage = Storage()

enum Test {
  @StorageBacked(key: "flag", storage: storage, defaultValue: false)
  nonisolated(unsafe) static var flag: Bool // Ok
}
