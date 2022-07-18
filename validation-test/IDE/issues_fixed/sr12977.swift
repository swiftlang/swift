// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

struct ButtonStyleConfiguration {}

protocol ButtonStyle {
  typealias Configuration = ButtonStyleConfiguration
}

struct BorderedBarButtonStyle: ButtonStyle {
  init() { }
  func makeBody(configuration: #^COMPLETE^#) {}
}

// COMPLETE: Begin completions
// COMPLETE: Decl[TypeAlias]/Super:              Configuration[#ButtonStyleConfiguration#]; name=Configuration
// COMPLETE: End completions
