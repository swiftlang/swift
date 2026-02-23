// RUN: %target-run-simple-swift(-enable-experimental-feature SuppressedAssociatedTypesWithDefaults) \
// RUN:      | %FileCheck --check-prefix=EXEC %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_SuppressedAssociatedTypesWithDefaults

protocol P {
    func p()
}

protocol Q {
    func q()
}

protocol Style<Primary> {
  associatedtype Primary: ~Copyable
  associatedtype Secondary: ~Copyable
}

struct Hat<S: Style> where S.Primary: ~Copyable {}

extension Hat: P /*<implied> where S.Primary: Copyable */ {
    func p() {
        print("\(self) : P")
    }
}

extension Hat: Q where S.Primary: ~Copyable, S.Secondary: Copyable {
    func q() {
        print("\(self) : Q")
    }
}

struct NC: ~Copyable {}

struct AllCopyable: Style {
  typealias Primary = Int
  typealias Secondary = Int
}

struct NCPrimary: Style {
  typealias Primary = NC
  typealias Secondary = Int
}

struct NCSecondary: Style {
  typealias Primary = Int
  typealias Secondary = NC
}

struct NCBoth: Style {
  typealias Primary = NC
  typealias Secondary = NC
}


@inline(never)
func castP<T>(value: T) -> (any P)? {
    return value as? any P
}

@inline(never)
func castQ<T>(value: T) -> (any Q)? {
    return value as? any Q
}

func test<T>(_ t: T) {
  if let p = castP(value: t) {
    p.p()
  } else {
    print("\(t) is not a P")
  }
  if let q = castQ(value: t) {
    q.q()
  } else {
    print("\(t) is not a Q")
  }
}

func main() {
    // EXEC: going to test
    print("going to test")

    // EXEC-NEXT: Hat<AllCopyable>() : P
    // EXEC-NEXT: Hat<AllCopyable>() : Q
    test(Hat<AllCopyable>())

    // EXEC-NEXT: Hat<NCPrimary>() is not a P
    // EXEC-NEXT: Hat<NCPrimary>() : Q
    test(Hat<NCPrimary>())

    // EXEC-NEXT: Hat<NCSecondary>() : P
    // EXEC-NEXT: Hat<NCSecondary>() is not a Q
    test(Hat<NCSecondary>())

    // EXEC-NEXT: Hat<NCBoth>() is not a P
    // EXEC-NEXT: Hat<NCBoth>() is not a Q
    test(Hat<NCBoth>())

    // EXEC-NEXT: done testing
    print("done testing")
}
main()
