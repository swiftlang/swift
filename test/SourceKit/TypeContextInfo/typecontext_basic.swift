enum Direction {
  case east, west
  case vector(x: Int, y: Int)
  case distance(Int)
}
struct Target : OptionSet {
  /// Mine.
  static let me: Target = .init(rawValue: 1 << 0)
  /// Yours.
  static let you: Target = .init(rawValue: 1 << 1)
  /// Theirs.
  static let them: Target = .init(rawValue: 1 << 2)
  /// One for all.
  static var all: Target {
    return [.me, .you, .them]
  }
}

class C {
  func foo(x: Direction) {}
  func foo(x: Target) {}
}

func test(obj: C) {
  let _ = obj.foo(x: 
}

// RUN: %sourcekitd-test -req=typecontextinfo -pos=25:22 %s -- %s > %t.response
// RUN: diff -u %s.response %t.response
