// RUN: %empty-directory(%t)
// RUN: %validate-incrparse %s --test-case INSERT_SPACE

class AnimationType {
  func foo(x: Blah) {
    switch x {
    case (.

extension AnimationType {
  public<<INSERT_SPACE<||| >>> 
