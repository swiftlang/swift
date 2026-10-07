// RUN: %target-swift-frontend -emit-sil %s -swift-version 6 -target %target-future-triple -enable-experimental-feature NoncopyableCasting -o /dev/null
// RUN: %target-swift-frontend -emit-sil %s -swift-version 6 -target %target-future-triple -enable-experimental-feature NoncopyableCasting -enable-upcoming-feature NonisolatedNonsendingByDefault -o /dev/null

// REQUIRES: concurrency
// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// Region analysis reads a `checked_cast_addr_br` destination as operand 1, which
// a `test_only` cast does not have. The plain-function case is covered by
// noncopyable_existential_is_region_analysis.swift; this file pushes on the
// shapes where the isolation machinery has the most to say: `sending`
// parameters, actor-isolated subjects, and values crossing an isolation
// boundary.
//
// Nothing here should be diagnosed. The point is that requesting region
// analysis over these does not crash.

protocol P: ~Copyable {}

struct NC: ~Copyable, P {
  var t: Int
}

struct Other: ~Copyable, P {}

final class Ref: Sendable {}

protocol Q {}
struct CS: Q {}

// MARK: - sending parameters

func sendingCopyable(_ x: sending any Q) -> Bool {
  x is CS
}

func sendingGeneric<T>(_ x: sending T) -> Bool {
  x is CS
}

func sendingResult(_ x: sending any Q) -> sending any Q {
  _ = x is CS
  return x
}

// MARK: - actor-isolated subjects

actor A {
  let box: any Q = CS()
  var generic: any Q = CS()

  func testStored() -> Bool {
    box is CS
  }

  func testMutable() -> Bool {
    generic is CS
  }

  func testNoncopyable(_ b: borrowing any P & ~Copyable) -> Bool {
    b is NC
  }

  func testInSwitch(_ b: borrowing any P & ~Copyable) -> Int {
    switch b {
    case is NC: return 1
    case is Other: return 2
    default: return 0
    }
  }
}

// MARK: - crossing an isolation boundary

@MainActor
func mainActorTest(_ x: any Q) -> Bool {
  x is CS
}

func awaitAcross(_ a: A, _ b: borrowing any P & ~Copyable) async -> Bool {
  // A test on a borrowed noncopyable existential either side of a suspension.
  let before = b is NC
  _ = await a.testStored()
  let after = b is NC
  return before == after
}

// The subject is created inside the concurrent context, so nothing non-Sendable
// crosses a boundary; the point is that the cast is translated there at all.
func inTaskGroup() async -> Bool {
  await withTaskGroup(of: Bool.self) { group in
    group.addTask {
      let x: any Q = CS()
      return x is CS
    }
    var r = false
    for await v in group { r = r || v }
    return r
  }
}

func inAsyncLet() async -> Bool {
  async let a: Bool = {
    let x: any Q = CS()
    return x is CS
  }()
  return await a
}

// MARK: - noncopyable subject in an async function

func asyncNoncopyable(_ b: consuming any P & ~Copyable) async -> Int {
  if case is NC = b { return 1 }
  return 0
}
