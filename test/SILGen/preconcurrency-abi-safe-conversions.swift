// RUN: %target-swift-emit-silgen -swift-version 5 -verify %s
// RUN: %target-swift-emit-silgen -swift-version 6 -verify %s

struct Text {
    init<S>(_: S) where S: StringProtocol {}
}

// In Swift 5, we introduce an implicit @Sendable on the closures here.
// Make sure that doing so doesn't disrupt SILGen's lvalue emission.
// rdar://130016855
public struct Header<TitleContent> {
  @preconcurrency
  private let titleContent:  @MainActor () -> TitleContent

  init(title: String) where TitleContent == Text {
    self.titleContent = {
      Text(title)
    }
  }

  func testGet() -> @MainActor () -> Text
      where TitleContent == Text {
    return titleContent // expected-warning * {{}}
  }
}
