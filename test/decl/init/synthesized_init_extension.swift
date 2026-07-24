// RUN: %target-typecheck-verify-swift %S/Inputs/synthesized_init_extension_other.swift

// https://github.com/swiftlang/swift/issues/90519
// An explicit initializer declared in a same-file extension takes the place
// of the synthesized memberwise or default initializer it would otherwise
// conflict with.

public struct Empty {}

extension Empty {
  public init() {}
}

public struct Memberwise {
  public var x: Int
  public var y: String
}

extension Memberwise {
  public init(x: Int, y: String) {
    self.x = x
    self.y = y
  }
}

func useSuppressed() {
  let _ = Empty()
  let _ = Memberwise(x: 1, y: "two")
}

// An initializer with a different signature keeps the synthesized memberwise
// initializer around.
struct KeepsMemberwise {
  var x: Int
}

extension KeepsMemberwise {
  init(string: String) {
    self.init(x: Int(string) ?? 0)
  }
}

// Same labels but different parameter types overload the memberwise
// initializer rather than replacing it.
struct KeepsMemberwiseOverload {
  var x: Int
}

extension KeepsMemberwiseOverload {
  init(x: String) {
    self.init(x: Int(x) ?? 0)
  }
}

func useKeepsMemberwise() {
  let _ = KeepsMemberwise(x: 1)
  let _ = KeepsMemberwise(string: "2")
  let _ = KeepsMemberwiseOverload(x: 1)
  let _ = KeepsMemberwiseOverload(x: "2")
}

struct Generic<T> {
  var value: T
}

extension Generic {
  init(value: T) {
    self.value = value
  }
}

// An initializer in a constrained extension has a different signature and
// coexists with the synthesized memberwise initializer.
struct Constrained<T> {
  var value: T
}

extension Constrained where T == Int {
  init(value: Int) {
    self.value = value
  }
}

func useGeneric() {
  let _ = Generic(value: 1)
  let _ = Constrained(value: "hi")
}

// An async initializer does not conflict with the synthesized initializer,
// so both coexist as overloads.
struct AsyncOverload {}

extension AsyncOverload {
  init() async {}
}

func useAsyncOverload() {
  let _ = AsyncOverload()
}

// A failable or throwing initializer still takes the synthesized
// initializer's place, since it would still conflict.
struct FailableThrowingEmpty {}

extension FailableThrowingEmpty {
  init?() throws {}
}

func useFailableThrowing() throws {
  let _: FailableThrowingEmpty? = try FailableThrowingEmpty()
}

// An initializer in an extension in a different file still conflicts.

extension CrossFileMemberwise {
  init(x: Int) { self.x = x } // expected-error {{invalid redeclaration of synthesized initializer 'init(x:)'}}
}

extension CrossFileEmpty {
  init() {} // expected-error {{invalid redeclaration of synthesized initializer 'init()'}}
}
