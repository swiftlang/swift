// RUN: %empty-directory(%t)
// Opened-existential archetypes carry a UUID that changes across compiles,
// so each pipeline normalizes them before diffing.
//
// File-level `using @MainActor` should produce the same SIL as writing
// `@MainActor` explicitly on every top-level declaration (including
// extensions, which is where it diverges from module-level `-default-isolation
// MainActor` since SE-0466 carve-outs do not apply at file scope).

// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 5 -disable-availability-checking -module-name equivalence -DFILE_DEFAULT %s | %{python} %S/Inputs/normalize_sil_uuids.py > %t/file-5.sil
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 5 -disable-availability-checking -module-name equivalence -DEXPLICIT %s | %{python} %S/Inputs/normalize_sil_uuids.py > %t/explicit-5.sil
// RUN: diff %t/file-5.sil %t/explicit-5.sil

// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 5 -strict-concurrency=complete -disable-availability-checking -module-name equivalence -DFILE_DEFAULT %s | %{python} %S/Inputs/normalize_sil_uuids.py > %t/file-5c.sil
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 5 -strict-concurrency=complete -disable-availability-checking -module-name equivalence -DEXPLICIT %s | %{python} %S/Inputs/normalize_sil_uuids.py > %t/explicit-5c.sil
// RUN: diff %t/file-5c.sil %t/explicit-5c.sil

// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 6 -disable-availability-checking -module-name equivalence -DFILE_DEFAULT %s | %{python} %S/Inputs/normalize_sil_uuids.py > %t/file-6.sil
// RUN: %target-swift-frontend -enable-experimental-feature DefaultIsolationPerFile -emit-sil -swift-version 6 -disable-availability-checking -module-name equivalence -DEXPLICIT %s | %{python} %S/Inputs/normalize_sil_uuids.py > %t/explicit-6.sil
// RUN: diff %t/file-6.sil %t/explicit-6.sil

// REQUIRES: concurrency
// REQUIRES: swift_feature_DefaultIsolationPerFile

#if FILE_DEFAULT
using @MainActor
#endif

#if EXPLICIT
@MainActor
#endif
func freeFunc() {}

#if EXPLICIT
@MainActor
#endif
func asyncFunc() async {
  Task { freeFunc() }
  Task.detached { }
}

#if EXPLICIT
@MainActor
#endif
class C {
  static let shared = C()
  static func factory() -> C { C() }

  init() {}
  func method() {}
  let storedLet = 0

  var observed: Int = 0 {
    willSet { _ = newValue }
    didSet { _ = oldValue }
  }

  subscript(index: Int) -> Int {
    get { index }
    set { _ = newValue }
  }

  nonisolated func nonisolatedMember() {}

  // Synthetic deinit inherits default isolation but not decl isolation, explicit does not, via SE-0371.
  deinit {}
}

#if EXPLICIT
@MainActor
#endif
class Box<T> {
  var value: T
  init(_ value: T) { self.value = value }

  deinit {}
}

#if EXPLICIT
@MainActor
#endif
struct S {
  var value: Int
  func method() {}
}

#if EXPLICIT
@MainActor
#endif
struct Pair<First, Second> {
  var first: First
  var second: Second
}

#if EXPLICIT
@MainActor
#endif
extension S {
  func extensionMethod() {}
  static func staticExtensionMethod() {}
  var computedProperty: Int { value }
  static var staticComputedProperty: Int { 0 }
  subscript(key: String) -> Int { 0 }
}

#if EXPLICIT
@MainActor
#endif
enum E {
  case one
  case two
  func method() {}
}

#if EXPLICIT
@MainActor
#endif
enum Assoc {
  case int(Int)
  case pair(Int, String)
  func describe() -> String { "" }
}

#if EXPLICIT
@MainActor
#endif
protocol P {
  func requirement()
}

// A typealias can't have an isolation.
typealias IntAlias = Int

// Actors keep their isolation!
actor A {
  var state: Int = 0
  func method() {}
}

// Isolation doesn't make sense for a precedence group.
precedencegroup ForwardApplication {
  associativity: left
  higherThan: AssignmentPrecedence
}

// Or an operator.
infix operator |>: ForwardApplication

#if EXPLICIT
@MainActor
#endif
func |><T, U>(value: T, transform: (T) -> U) -> U {
  transform(value)
}

@concurrent
func concurrentFunc() async {}

nonisolated func nonisolatedFunc() {}
