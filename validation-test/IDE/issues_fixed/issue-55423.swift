// RUN: %batch-code-completion

// https://github.com/apple/swift/issues/55423

struct ButtonStyleConfiguration {}

protocol ButtonStyle {
  typealias Configuration = ButtonStyleConfiguration
}

struct BorderedBarButtonStyle: ButtonStyle {
  init() { }
  func makeBody(configuration: #^COMPLETE^#) {}
}

// COMPLETE: Decl[TypeAlias]/Super:              Configuration[#ButtonStyleConfiguration#]; name=Configuration
