// RUN: %target-run-simple-swift(-enable-experimental-feature SuppressedAssociatedTypesWithDefaults) \
// RUN:      | %FileCheck --check-prefix=EXEC %s

// RUN: %target-run-simple-swift(-enable-experimental-feature SuppressedAssociatedTypes) \
// RUN:      | %FileCheck --check-prefix=EXEC %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_SuppressedAssociatedTypesWithDefaults
// REQUIRES: swift_feature_SuppressedAssociatedTypes

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

protocol BothC {
    func bothC()
}

protocol PrimaryC {
    func primaryC()
}

protocol SecondC {
    func secondC()
}

protocol NeitherC {
    func neitherC()
}

protocol Style<Primary> {
  associatedtype Primary: ~Copyable
  associatedtype Secondary: ~Copyable
}

struct Hat<S: Style> where S.Primary: ~Copyable {}

// Require both to be Copyable
extension Hat: BothC where S.Primary: Copyable, S.Secondary: Copyable {
    func bothC() {
        print("\(self) : BothC")
    }
}

// Require only Primary to be Copyable
extension Hat: PrimaryC where S.Primary: Copyable, S.Secondary: ~Copyable {
    func primaryC() {
        print("\(self) : PrimaryC")
    }
}

// Require only Secondary to be Copyable
extension Hat: SecondC where S.Primary: ~Copyable, S.Secondary: Copyable {
    func secondC() {
        print("\(self) : SecondC")
    }
}

// Require neither to be Copyable
extension Hat: NeitherC where S.Primary: ~Copyable, S.Secondary: ~Copyable {
  func neitherC() {
    print("\(self) : NeitherC")
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
func castBothC<T>(_: T.Type) where T: Style, T.Primary: ~Copyable {
  let value = Hat<T>()
  if let x = value as? any BothC {
    x.bothC()
  } else {
    print("\(value) is not a BothC")
  }
}

@inline(never)
func castPrimaryC<T>(_: T.Type) where T: Style, T.Primary: ~Copyable {
  let value = Hat<T>()
  if let x = value as? any PrimaryC {
    x.primaryC()
  } else {
    print("\(value) is not a PrimaryC")
  }
}

@inline(never)
func castSecondC<T>(_: T.Type) where T: Style, T.Primary: ~Copyable {
  let value = Hat<T>()
  if let x = value as? any SecondC {
    x.secondC()
  } else {
    print("\(value) is not a SecondC")
  }
}

@inline(never)
func castNeitherC<T>(_: T.Type) where T: Style, T.Primary: ~Copyable {
  let value = Hat<T>()
  if let x = value as? any NeitherC {
    x.neitherC()
  } else {
    print("\(value) is not a NeitherC")
  }
}

// Additional test where conformed-to protocol itself has a Copyable associated type.
protocol HasACopyableAssoc {
  associatedtype Inner
  func describe()
}

struct G<T: Style> where T.Primary: ~Copyable {}

extension G: HasACopyableAssoc where T.Primary: Copyable {
  typealias Inner = T.Primary
  func describe() { print("G<\(T.self)<\(Inner.self), _>> : HasACopyableAssoc") }
}

@inline(never)
func castHasA<T>(_: T.Type) where T: Style, T.Primary: ~Copyable {
  let value = G<T>()
  if let x = value as? any HasACopyableAssoc {
    x.describe()
  } else {
    print("G<\(T.self)<\(T.Primary.self), _>> is not a HasACopyableAssoc")
  }
}


func main() {
    // EXEC: going to test
    print("going to test")

    // EXEC-NEXT: Hat<AllCopyable>() : BothC
    // EXEC-NEXT: Hat<AllCopyable>() : PrimaryC
    // EXEC-NEXT: Hat<AllCopyable>() : SecondC
    // EXEC-NEXT: Hat<AllCopyable>() : NeitherC
    castBothC(AllCopyable.self)
    castPrimaryC(AllCopyable.self)
    castSecondC(AllCopyable.self)
    castNeitherC(AllCopyable.self)

    // EXEC-NEXT: Hat<NCPrimary>() is not a BothC
    // EXEC-NEXT: Hat<NCPrimary>() is not a PrimaryC
    // EXEC-NEXT: Hat<NCPrimary>() : SecondC
    // EXEC-NEXT: Hat<NCPrimary>() : NeitherC
    castBothC(NCPrimary.self)
    castPrimaryC(NCPrimary.self)
    castSecondC(NCPrimary.self)
    castNeitherC(NCPrimary.self)

    // EXEC-NEXT: Hat<NCSecondary>() is not a BothC
    // EXEC-NEXT: Hat<NCSecondary>() : PrimaryC
    // EXEC-NEXT: Hat<NCSecondary>() is not a SecondC
    // EXEC-NEXT: Hat<NCSecondary>() : NeitherC
    castBothC(NCSecondary.self)
    castPrimaryC(NCSecondary.self)
    castSecondC(NCSecondary.self)
    castNeitherC(NCSecondary.self)

    // EXEC-NEXT: Hat<NCBoth>() is not a BothC
    // EXEC-NEXT: Hat<NCBoth>() is not a PrimaryC
    // EXEC-NEXT: Hat<NCBoth>() is not a SecondC
    // EXEC-NEXT: Hat<NCBoth>() : NeitherC
    castBothC(NCBoth.self)
    castPrimaryC(NCBoth.self)
    castSecondC(NCBoth.self)
    castNeitherC(NCBoth.self)

    // EXEC-NEXT: G<AllCopyable<Int, _>> : HasACopyableAssoc
    // EXEC-NEXT: G<NCPrimary<NC, _>> is not a HasACopyableAssoc
    // EXEC-NEXT: G<NCSecondary<Int, _>> : HasACopyableAssoc
    // EXEC-NEXT: G<NCBoth<NC, _>> is not a HasACopyableAssoc
    castHasA(AllCopyable.self)
    castHasA(NCPrimary.self)
    castHasA(NCSecondary.self)
    castHasA(NCBoth.self)

    // EXEC-NEXT: done testing
    print("done testing")
}
main()
