// RUN: %target-run-simple-swift(-parse-as-library  -Xfrontend -disable-availability-checking -Xfrontend -concurrency-model=task-to-thread) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: freestanding
// REQUIRES: concurrency_runtime

@_spi(_TaskToThreadModel) import _Concurrency

// =============================================================================
// Driver                                                                     {{
// =============================================================================
@main struct Main {
  // Note that this isn't and can't be async.
  static func main() {
    // Test some of the following combinations
    // - closure context: no, immutable, mutable
    // - returns:         void, trivial, nontrivial, gigantic
    // - throws:          can't, no, yes(trivial), yes(nontrivial)
    // - callee:          no, sync, async

    // 0000
    NoClosureContext_VoidReturn_CantThrow_NoCallee()
    // 0100
    NoClosureContext_TrivialReturn_CantThrow_NoCallee()

    // 1000
    ImmutableClosureContext_VoidReturn_CantThrow_NoCallee()
    // 1012
    ImmutableClosureContext_VoidReturn_NoThrow_AsyncCallee()
    // 1021
    ImmutableClosureContext_VoidReturn_TrivialThrow_SyncCallee()
    // 1101
    ImmutableClosureContext_TrivialReturn_CantThrow_SyncCallee()
    // 1301
    ImmutableClosureContext_GiganticReturn_CantThrow_SyncCallee()

    // 2000
    MutableClosureContext_VoidReturn_CantThrow_NoCallee()
    // 2212
    MutableClosureContext_NontrivialReturn_NoThrow_AsyncCallee()
    // 2232
    MutableClosureContext_NontrivialReturn_NontrivialThrow_AsyncCallee()
  }
}
// =============================================================================
// Driver                                                                     }}
// =============================================================================

// =============================================================================
// Tests                                                                      {{
// =============================================================================
func NoClosureContext_VoidReturn_CantThrow_NoCallee() {
  // CHECK: NoClosureContext_VoidReturn_CantThrow_NoCallee() before
  // CHECK: NoClosureContext_VoidReturn_CantThrow_NoCallee() during
  // CHECK: NoClosureContext_VoidReturn_CantThrow_NoCallee() after
  print("\(#function) before")
  Task.runInline {
    print("\(#function) during")
  }
  print("\(#function) after")
}
func NoClosureContext_TrivialReturn_CantThrow_NoCallee() {
  // CHECK: NoClosureContext_TrivialReturn_CantThrow_NoCallee() before
  // CHECK: NoClosureContext_TrivialReturn_CantThrow_NoCallee() during
  // CHECK: NoClosureContext_TrivialReturn_CantThrow_NoCallee() result=Trivial(context: "NoClosureContext_TrivialReturn_CantThrow_NoCallee()")
  // CHECK: NoClosureContext_TrivialReturn_CantThrow_NoCallee() after
  print("\(#function) before")
  let result = Task.runInline {
    print("\(#function) during")
    return Trivial(#function)
  }
  print("\(#function) result=\(result)")
  print("\(#function) after")
}

func ImmutableClosureContext_VoidReturn_CantThrow_NoCallee() {
  // CHECK: ImmutableClosureContext_VoidReturn_CantThrow_NoCallee() before: immutable
  // CHECK: ImmutableClosureContext_VoidReturn_CantThrow_NoCallee() during: immutable
  // CHECK: ImmutableClosureContext_VoidReturn_CantThrow_NoCallee() after: immutable
  let immutableField = "immutable"
  print("\(#function) before: \(immutableField)")
  Task.runInline {
    print("\(#function) during: \(immutableField)")
  }
  print("\(#function) after: \(immutableField)")
}

func ImmutableClosureContext_VoidReturn_NoThrow_AsyncCallee() {
  // CHECK: ImmutableClosureContext_VoidReturn_NoThrow_AsyncCallee() before: immutable
  // CHECK: ImmutableClosureContext_VoidReturn_NoThrow_AsyncCallee() during: immutable
  // CHECK: CalleeAsync_NoThrow_VoidReturn()
  // CHECK: ImmutableClosureContext_VoidReturn_NoThrow_AsyncCallee() result=success()
  // CHECK: ImmutableClosureContext_VoidReturn_NoThrow_AsyncCallee() after: immutable
  let immutableField = "immutable"
  print("\(#function) before: \(immutableField)")
  let result = Result {
    try Task.runInline {
      print("\(#function) during: \(immutableField)")
      return try await CalleeAsync_NoThrow_VoidReturn()
    }
  }
  print("\(#function) result=\(result)")
  print("\(#function) after: \(immutableField)")
}

func ImmutableClosureContext_VoidReturn_TrivialThrow_SyncCallee() {
  // CHECK: ImmutableClosureContext_VoidReturn_TrivialThrow_SyncCallee() before: immutable
  // CHECK: ImmutableClosureContext_VoidReturn_TrivialThrow_SyncCallee() during: immutable
  // CHECK: CalleeSync_TrivialThrow_VoidReturn()
  // CHECK: ImmutableClosureContext_VoidReturn_TrivialThrow_SyncCallee() result=failure(ETrivial.trivial(Trivial(context: "CalleeSync_TrivialThrow_VoidReturn()")))
  // CHECK: ImmutableClosureContext_VoidReturn_TrivialThrow_SyncCallee() after: immutable
  let immutableField = "immutable"
  print("\(#function) before: \(immutableField)")
  let result = Result {
    try Task.runInline {
      print("\(#function) during: \(immutableField)")
      try CalleeSync_TrivialThrow_VoidReturn()
    }
  }
  print("\(#function) result=\(result)")
  print("\(#function) after: \(immutableField)")
}

func ImmutableClosureContext_TrivialReturn_CantThrow_SyncCallee() {
  // CHECK: ImmutableClosureContext_TrivialReturn_CantThrow_SyncCallee() before: immutable
  // CHECK: ImmutableClosureContext_TrivialReturn_CantThrow_SyncCallee() during: immutable
  // CHECK: CalleeSync_CantThrow_TrivialReturn()
  // CHECK: ImmutableClosureContext_TrivialReturn_CantThrow_SyncCallee() result=Trivial(context: "CalleeSync_CantThrow_TrivialReturn()")
  // CHECK: ImmutableClosureContext_TrivialReturn_CantThrow_SyncCallee() after: immutable
  let immutableField = "immutable"
  print("\(#function) before: \(immutableField)")
  let result = Task.runInline {
    print("\(#function) during: \(immutableField)")
    return CalleeSync_CantThrow_TrivialReturn()
  }
  print("\(#function) result=\(result)")
  print("\(#function) after: \(immutableField)")
}

func ImmutableClosureContext_GiganticReturn_CantThrow_SyncCallee() {
  // CHECK: ImmutableClosureContext_GiganticReturn_CantThrow_SyncCallee() before: immutable
  // CHECK: ImmutableClosureContext_GiganticReturn_CantThrow_SyncCallee() during: immutable
  // CHECK: CalleeSync_CantThrow_GiganticReturn()
  // CHECK: ImmutableClosureContext_GiganticReturn_CantThrow_SyncCallee() result=Gigantic(context: "CalleeSync_CantThrow_GiganticReturn()"
  // CHECK: ImmutableClosureContext_GiganticReturn_CantThrow_SyncCallee() after: immutable
  let immutableField = "immutable"
  print("\(#function) before: \(immutableField)")
  let result = Task.runInline {
    print("\(#function) during: \(immutableField)")
    return CalleeSync_CantThrow_GiganticReturn()
  }
  print("\(#function) result=\(result)")
  print("\(#function) after: \(immutableField)")
}

func MutableClosureContext_VoidReturn_CantThrow_NoCallee() {
  // CHECK: MutableClosureContext_VoidReturn_CantThrow_NoCallee() before: unchanged
  // CHECK: MutableClosureContext_VoidReturn_CantThrow_NoCallee() during: unchanged
  // CHECK: MutableClosureContext_VoidReturn_CantThrow_NoCallee() after: changed
  var mutableField = "unchanged"
  print("\(#function) before: \(mutableField)")
  Task.runInline {
    print("\(#function) during: \(mutableField)")
    mutableField = "changed"
  }
  print("\(#function) after: \(mutableField)")
}

func MutableClosureContext_NontrivialReturn_NoThrow_AsyncCallee() {
  // CHECK: MutableClosureContext_NontrivialReturn_NoThrow_AsyncCallee() before: unchanged
  // CHECK: MutableClosureContext_NontrivialReturn_NoThrow_AsyncCallee() during: unchanged
  // CHECK: X.init(CalleeAsync_NoThrow_NontrivialReturn()|retval)
  // CHECK: MutableClosureContext_NontrivialReturn_NoThrow_AsyncCallee() result=success(Nontrivial(context: "CalleeAsync_NoThrow_NontrivialReturn()|retval"))
  // CHECK: X.deinit(CalleeAsync_NoThrow_NontrivialReturn()|retval)
  // CHECK: MutableClosureContext_NontrivialReturn_NoThrow_AsyncCallee() after: changed
  var mutableField = "unchanged"
  print("\(#function) before: \(mutableField)")
  do {
    let result = Result {
      try Task.runInline {
        print("\(#function) during: \(mutableField)")
        mutableField = "changed"
        return try await CalleeAsync_NoThrow_NontrivialReturn()
      }
    }
    print("\(#function) result=\(result)")
  }
  print("\(#function) after: \(mutableField)")
}

func MutableClosureContext_NontrivialReturn_NontrivialThrow_AsyncCallee() {
  // CHECK: MutableClosureContext_NontrivialReturn_NontrivialThrow_AsyncCallee() before: unchanged
  // CHECK: MutableClosureContext_NontrivialReturn_NontrivialThrow_AsyncCallee() during: unchanged
  // CHECK: X.init(CalleeAsync_NontrivialThrow_NontrivialReturn()|retval)
  // CHECK: X.init(CalleeAsync_NontrivialThrow_NontrivialReturn()|error)
  // CHECK: X.deinit(CalleeAsync_NontrivialThrow_NontrivialReturn()|retval)
  // CHECK: MutableClosureContext_NontrivialReturn_NontrivialThrow_AsyncCallee() result=failure(ENontrivial.nontrivial(Nontrivial(context: "CalleeAsync_NontrivialThrow_NontrivialReturn()|error")))
  // CHECK: X.deinit(CalleeAsync_NontrivialThrow_NontrivialReturn()|error)
  // CHECK: MutableClosureContext_NontrivialReturn_NontrivialThrow_AsyncCallee() after: changed
  var mutableField = "unchanged"
  print("\(#function) before: \(mutableField)")
  do {
    let result = Result {
      try Task.runInline {
        print("\(#function) during: \(mutableField)")
        mutableField = "changed"
        return try await CalleeAsync_NontrivialThrow_NontrivialReturn()
      }
    }
    print("\(#function) result=\(result)")
  }
  print("\(#function) after: \(mutableField)")
}
// =============================================================================
// Tests                                                                      }}
// =============================================================================

// =============================================================================
// Callees                                                                    {{
// =============================================================================
// Some of the combinations of the following:
// - synchroneity: sync, async
// - throwness: can't, no, trivial, nontrivial
// - return: void, trivial, nontrivial, gigantic

// 000
func CalleeSync_CantThrow_VoidReturn() -> () {
  print(#function)
}

// 020
func CalleeSync_TrivialThrow_VoidReturn() throws -> () {
  print(#function)
  throw ETrivial.trivial(Trivial(#function))
}

// 002
func CalleeSync_CantThrow_TrivialReturn() -> Trivial {
  print(#function)
  return Trivial(#function)
}

func CalleeSync_CantThrow_GiganticReturn() -> Gigantic {
  print(#function)
  return Gigantic(#function)
}

// 110
func CalleeAsync_NoThrow_VoidReturn() async throws -> () {
  print(#function)
}

// 112
func CalleeAsync_NoThrow_NontrivialReturn() async throws -> Nontrivial {
  print(#function)
  let retval = Nontrivial("\(#function)|retval")
  return retval
}

// 132
func CalleeAsync_NontrivialThrow_NontrivialReturn() async throws -> Nontrivial {
  print(#function)
  let retval = Nontrivial("\(#function)|retval")
  _ = retval
  throw ENontrivial.nontrivial(Nontrivial("\(#function)|error"))
}
// =============================================================================
// Callees                                                                    }}
// =============================================================================

// =============================================================================
// Types                                                                      {{
// =============================================================================
class X {
  var context: String
  init(_ context: String) {
    self.context = context
    print("X.init(\(context))")
  }
  deinit {
    print("X.deinit(\(self.context))")
  }
}
struct Gigantic : CustomStringConvertible {
  var description: String {
    return "Gigantic(context: \"\(context)\")"
  }
  struct One {
    var f0: Int = 111
    var f1: Int = 111
    var f2: Int = 111
    var f3: Int = 111
    var f4: Int = 111
    var f5: Int = 111
    var f6: Int = 111
    var f7: Int = 111
    var f8: Int = 111
    var f9: Int = 111
    var f10: Int = 111
    var f11: Int = 111
    var f12: Int = 111
    var f13: Int = 111
    var f14: Int = 111
    var f15: Int = 111
  }
  struct Two {
    var f0: One = One()
    var f1: One = One()
    var f2: One = One()
    var f3: One = One()
    var f4: One = One()
    var f5: One = One()
    var f6: One = One()
    var f7: One = One()
    var f8: One = One()
    var f9: One = One()
    var f10: One = One()
    var f11: One = One()
    var f12: One = One()
    var f13: One = One()
    var f14: One = One()
    var f15: One = One()
  }
  var context: String
  init(_ context: String) {
    self.context = context
    self.f0 = Two()
    self.f1 = Two()
  }
  var f0: Two
  var f1: Two
}
extension Result : CustomStringConvertible {
  public var description: String {
    switch self {
      case .success(let success):
      if success is Void {
        return "success()"
      } else if let csc = success as? CustomStringConvertible {
        return "success(\(csc.description))"
      } else {
        return "success(OPAQUE)"
      }
      case .failure(let failure):
      if let csc = failure as? CustomStringConvertible {
        return "failure(\(csc.description))"
      } else {
        return "failure(OPAQUE)"
      }
    }
  }
}
struct Nontrivial : CustomStringConvertible {
  var description: String {
    return "Nontrivial(context: \"\(context)\")"
  }
  var context: String
  init(_ context: String) {
    self.context = context
    self.f0 = X(context)
    self.f1 = 111
    self.f2 = 111
    self.f3 = 111
    self.f4 = 111
    self.f5 = 111
    self.f6 = 111
    self.f7 = 111
    self.f8 = 111
    self.f9 = 111
  }
  var f0: X
  var f1: Int
  var f2: Int
  var f3: Int
  var f4: Int
  var f5: Int
  var f6: Int
  var f7: Int
  var f8: Int
  var f9: Int
}
struct Trivial : CustomStringConvertible {
  var description: String {
    return "Trivial(context: \"\(context)\")"
  }
  var context: String
  init(_ context: String) {
    self.context = context
    self.f0 = 111
    self.f1 = 111
    self.f2 = 111
    self.f3 = 111
    self.f4 = 111
    self.f5 = 111
    self.f6 = 111
    self.f7 = 111
    self.f8 = 111
    self.f9 = 111
  }
  var f0: Int
  var f1: Int
  var f2: Int
  var f3: Int
  var f4: Int
  var f5: Int
  var f6: Int
  var f7: Int
  var f8: Int
  var f9: Int
}
enum ETrivial : Error, CustomStringConvertible {
  var description: String {
    switch self {
    case .nada:
      return "ETrivial.nada"
    case .trivial(let trivial):
      return "ETrivial.trivial(\(trivial))"
    }
  }
  case nada
  case trivial(Trivial)
}
enum ENontrivial : Error, CustomStringConvertible {
  var description: String {
    switch self {
    case .nada:
      return "ENontrivial.nada"
    case .trivial(let trivial):
      return "ENontrivial.trivial(\(trivial))"
    case .nontrivial(let nontrivial):
      return "ENontrivial.nontrivial(\(nontrivial))"
    }
  }
  case nada
  case trivial(Trivial)
  case nontrivial(Nontrivial)
}
// =============================================================================
// Types                                                                      }}
// =============================================================================
