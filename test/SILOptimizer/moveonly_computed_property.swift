// RUN: %target-swift-frontend -emit-sil -verify %s > /dev/null



// Applying a computed property to a move-only field in a class should occur
// entirely within a formal access to the class property. rdar://105794506

struct FileDescriptor: ~Copyable {
  private let desc: Int

  var empty: Bool { return desc == Int.min }
}

final class Wrapper {
  private var val: FileDescriptor

  func isEmpty_bug() -> Bool {
    return val.empty
  }

  init() { fatalError() }
}
