// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all)
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all)

// REQUIRES: executable_test
// REQUIRES: asserts

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Needed due to limitations of autoclosures and noncopyable types.
func eagerAssert(_ b: Bool, _ line: Int = #line) {
  guard b else { fatalError("assertion failure on line \(line)") }
}

///---------------------
/// MARK: a Swift x Haskell limited-edition stdlib
/// ---------------------



/// MARK: Ord aka 'Comparable'
protocol Ord: ~Copyable {
  func compare(_ other: borrowing Self) -> Ordering
}

enum Ordering {
  case LT
  case EQ
  case GT

  static func compare<T: Comparable>(_ a: borrowing T, _ b: borrowing T) -> Ordering {
    if (a < b) { return .LT }
    if (a > b) { return .GT }
    eagerAssert(a == b)
    return .EQ
  }
}

extension Ord where Self: ~Copyable {
  func less(_ b: borrowing Self) -> Bool {
    return compare(b) == .LT
  }
  func lessOrEq(_ b: borrowing Self) -> Bool {
    return !greater(b)
  }
  func greater(_ b: borrowing Self) -> Bool {
    return compare(b) == .GT
  }
  func greaterOrEq(_ b: borrowing Self) -> Bool {
    return !less(b)
  }
  func equal(_ b: borrowing Self) -> Bool {
    compare(b) == .EQ
  }
  func notEqual(_ b: borrowing Self) -> Bool {
    !equal(b)
  }
}

/// MARK: Maybe aka 'Optional'
enum Maybe<Val: ~Copyable>: ~Copyable {
  case just(Val)
  case none

  // NOTE: a 'consuming' version of the unfortunately 'borrowing' map from the stdlib.
  consuming func consumingMap<U: ~Copyable>(_ fn: (consuming Val) throws -> U) rethrows -> Maybe<U> {
    // rdar://117638878 (MoveOnlyAddressChecker crashes with generic associated value in enum)
    fatalError("need to fix rdar://117638878")
//     return switch consume self {
//       case let .just(val): .just(try fn(val))
//       case .none: .none
//     }
  }
}

// NOTE: temporary
extension Maybe: Copyable where Val: Copyable {}


/// MARK: beginning of tests

struct FileDescriptor: ~Copyable, Ord {
  let id: Int

  func compare(_ other: borrowing FileDescriptor) -> Ordering {
    return .compare(id, other.id)
  }

  deinit { print("calling deinit!") }
}

class Shared<T: ~Copyable> {
  let value: T
  init(_ val: consuming T) { self.value = val }
  func borrow<V>(_ f: (borrowing T) throws -> V) rethrows -> V {
    return try f(value)
  }
}

defer { testOrd() }
func testOrd() {
  let stderr = Shared(FileDescriptor(id: 1))
  let stdout = Shared(FileDescriptor(id: 0))
  stdout.borrow { out in
    stderr.borrow { err in
      eagerAssert(err.greater(out))
      eagerAssert(out.less(err))
      eagerAssert(out.notEqual(err))
    }
  }
}

defer { testMaybe() }
func testMaybe() {
  let mayb = Maybe.just(FileDescriptor(id: 0));
  switch consume mayb {
    case let .just(fd): eagerAssert(fd.id == 0)
    case .none: eagerAssert(false)
  }
}
