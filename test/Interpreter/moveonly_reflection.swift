// RUN: %empty-directory(%t)

// RUN: %target-build-swift %s -o %t/exe
// RUN: %target-codesign %t/exe
// RUN: %target-run %t/exe > %t/output.txt
// RUN: %FileCheck %s < %t/output.txt

// REQUIRES: executable_test

enum Maybe<Wrapped: ~Copyable>: ~Copyable {
  case some(Wrapped)
  case none
}
extension Maybe: Copyable where Wrapped: Copyable {}

struct NC: ~Copyable {
  let data: Int
}

struct PairNC: ~Copyable {
  var x: Int
  var y: NC
}

enum NCEnum: ~Copyable {
  case leaf(Int)
  case pair(NC, Int)
}

// Conditionally Copyable; no T-typed storage.
struct FactoryOf<T: ~Copyable>: ~Copyable {
  var producedCount: Int = 42
}
extension FactoryOf: Copyable where T: Copyable {}

// Unconditionally noncopyable generic enum (single-payload).
enum NCBox<T: ~Copyable>: ~Copyable {
  case value(T)
  case empty
}

// Unconditionally noncopyable generic enum (multi-payload).
enum NCEither<L: ~Copyable, R: ~Copyable>: ~Copyable {
  case left(L)
  case right(R)
}

// Unconditionally noncopyable generic struct.
struct NCHolder<T: ~Copyable>: ~Copyable {
  var value: T
  var extra: Int
}

class Protected<T: ~Copyable> {
  var field: Maybe<T>
  init(_ t: consuming T) {
    self.field = .some(t)
  }
}

// We used to make an exception for stdlib types and permit reflection that can crash Mirror.
class Dangerous<T: ~Copyable> {
  var field: Optional<T>
  init(_ t: consuming T) {
    self.field = .some(t)
  }
}

class HoldsNCBox<T: ~Copyable> {
  var field: NCBox<T>
  init(_ t: consuming T) {
    self.field = .value(t)
  }
}

class HoldsNCEither<L: ~Copyable, R: ~Copyable> {
  var field: NCEither<L, R>
  init(left: consuming L) {
    self.field = .left(left)
  }
  init(right: consuming R) {
    self.field = .right(right)
  }
}

class HoldsNCHolder<T: ~Copyable> {
  var field: NCHolder<T>
  init(_ t: consuming T) {
    self.field = NCHolder(value: t, extra: 7)
  }
}

class MultiField {
  var name: String
  var value: NC
  var count: Int
  init(_ name: String, _ value: consuming NC, _ count: Int) {
    self.name = name
    self.value = value
    self.count = count
  }
}

class HoldsFactoryOf<T: ~Copyable> {
  var field: FactoryOf<T>
  init(_: consuming T.Type) {
    self.field = FactoryOf<T>()
  }
}

func printMirror<T: Copyable>(_ val: T) {
  let mirror = Mirror(reflecting: val)
  print("--- \(mirror.displayStyle.map { "\($0)" } ?? "none") ---")
  mirror.children.forEach { print($0.label ?? "?", $0.value) }
}

defer { test() }
func test() {
  // *** Struct/class fields containing noncopyable types
  // CHECK-LABEL: --- class ---
  // CHECK-NEXT: field some("oreo")
  printMirror(Protected("oreo"))

  // CHECK-LABEL: --- class ---
  // CHECK-NEXT: field ()
  printMirror(Protected(NC(data: 11)))

  // CHECK-LABEL: --- class ---
  // CHECK-NEXT: field Optional("spots")
  printMirror(Dangerous("spots"))

  // CHECK-LABEL: --- class ---
  // CHECK-NEXT: field ()
  printMirror(Dangerous(NC(data: 22)))

  // *** Class with multiple fields, some noncopyable.
  // CHECK-LABEL: --- class ---
  // CHECK-NEXT: name hello
  // CHECK-NEXT: value ()
  // CHECK-NEXT: count 42
  printMirror(MultiField("hello", NC(data: 7), 42))

  // *** Reflecting a conditionally-copyable struct field that is noncopyable
  // CHECK-LABEL: --- class ---
  // CHECK-NEXT: field ()
  printMirror(Protected(PairNC(x: 1, y: NC(data: 2))))

  // *** Copyable enum with copyable payload
  // CHECK-LABEL: --- optional ---
  // CHECK-NEXT: some cat
  printMirror(Optional<String>.some("cat"))

  // *** Copyable enum (Optional) with noncopyable payload
  // CHECK-LABEL: --- class ---
  // CHECK-NEXT: field ()
  printMirror(Dangerous(NC(data: 99)))

  // *** Noncopyable enum whose payload is copyable
  // CHECK-LABEL: --- class ---
  // CHECK-NEXT: field ()
  printMirror(Protected(NCEnum.leaf(5)))

  // *** Conditionally-copyable enum with copyable payload.
  // CHECK-LABEL: --- class ---
  // CHECK-NEXT: field some("hi")
  printMirror(Protected("hi"))

  // *** Always-noncopyable generic enum with copyable payload
  // CHECK-LABEL: --- class ---
  // CHECK-NEXT: field ()
  printMirror(HoldsNCBox("swift"))

  // *** Always-noncopyable multi-payload generic enum with copyable payloads
  // CHECK-LABEL: --- class ---
  // CHECK-NEXT: field ()
  printMirror(HoldsNCEither<String, Int>(left: "hello"))

  // CHECK-LABEL: --- class ---
  // CHECK-NEXT: field ()
  printMirror(HoldsNCEither<String, Int>(right: 42))

  // *** Always-noncopyable generic struct with copyable fields
  // CHECK-LABEL: --- class ---
  // CHECK-NEXT: field ()
  printMirror(HoldsNCHolder("world"))

  // *** Conditionally Copyable struct with no T-typed fields:
  // FactoryOf<String> is Copyable; FactoryOf<NC> is not.
  // CHECK-LABEL: --- class ---
  // CHECK-NEXT: field FactoryOf<String>(producedCount: 42)
  printMirror(HoldsFactoryOf(String.self))
  // CHECK-LABEL: --- class ---
  // CHECK-NEXT: field ()
  printMirror(HoldsFactoryOf(NC.self))
}
