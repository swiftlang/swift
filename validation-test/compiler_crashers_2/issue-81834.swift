// RUN: not --crash %target-swift-frontend -typecheck -library-level api %s
// REQUIRES: OS=macosx
// REQUIRES: asserts

// https://github.com/swiftlang/swift/issues/81834
public class C {
  public subscript() -> Int {
    get {
      extension C {}
