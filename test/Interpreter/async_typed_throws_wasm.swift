// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library -Onone %s -o %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: OS=wasip1

enum SmallErr: Error { case boom }

struct LargeErr: Error {
  var tag: Int
  var pad: (Int, Int, Int, Int)
}

struct LargeResult {
  var tag: Int
  var pad: (Int, Int, Int, Int, Int, Int, Int, Int)
}

func run<T, Failure: Error>(
  _ body: () async throws(Failure) -> T
) async -> Result<T, Failure> {
  do { return .success(try await body()) }
  catch { return .failure(error) }
}

func runIdent(_ body: () async -> String) async -> String {
  await body()
}

// For shape h: stored function pointer (forces non-FunctionRef SIL operand).
enum StoredClosure {
  static let boomClosure: () async throws(SmallErr) -> String = {
    throw SmallErr.boom
  }
}

// For shape j: witness-method async typed-throws.
protocol AsyncBoom {
  associatedtype Failure: Error
  func boom() async throws(Failure) -> String
}

struct BoomThrower: AsyncBoom {
  typealias Failure = SmallErr
  func boom() async throws(SmallErr) -> String { throw .boom }
}

func invokeWitness<T: AsyncBoom>(_ x: T) async -> Result<String, T.Failure> {
  do { return .success(try await x.boom()) }
  catch { return .failure(error) }
}

// For shape k: Thick self-context (actor method invoking a typed-throws closure).
actor BoomActor {
  func run() async -> Result<String, SmallErr> {
    do {
      return .success(try await { () async throws(SmallErr) -> String in
        throw SmallErr.boom
      }())
    } catch {
      return .failure(error)
    }
  }
}

@main
struct Main {
  static func main() async {
    // a: untyped throws (the issue #89320 repro shape).
    let a = await run { () async throws -> String in throw SmallErr.boom }
    switch a {
    case .success(let s): print("a-ok: \(s)")
    case .failure(let e): print("a-err: \(e)")
    }
    // CHECK: a-err: boom

    // b: typed throws, small error enum.
    let b = await run { () async throws(SmallErr) -> String in
      throw SmallErr.boom
    }
    switch b {
    case .success(let s): print("b-ok: \(s)")
    case .failure(let e): print("b-err: \(e)")
    }
    // CHECK-NEXT: b-err: boom

    // c: typed throws, large error struct (forces ind_error indirection).
    let c = await run { () async throws(LargeErr) -> String in
      throw LargeErr(tag: 42, pad: (0, 0, 0, 0))
    }
    switch c {
    case .success(let s): print("c-ok: \(s)")
    case .failure(let e): print("c-err: tag=\(e.tag)")
    }
    // CHECK-NEXT: c-err: tag=42

    // d: success path, no throws taken.
    let d = await run { () async throws -> String in "yay" }
    switch d {
    case .success(let s): print("d-ok: \(s)")
    case .failure(let e): print("d-err: \(e)")
    }
    // CHECK-NEXT: d-ok: yay

    // e: non-throws closure passed through a t2t, exercises the
    // predicate's negative branch (no indirect error result, so predicate
    // returns false and no wrapper is emitted; default lowering).
    let e = await runIdent { () async -> String in "noerror" }
    print("e-ok: \(e)")
    // CHECK-NEXT: e-ok: noerror

    // f: generic async typed-throws via explicit type argument
    // (R3 — polymorphic SIL function type rejected by SIL workaround).
    let f: Result<String, SmallErr> = await run {
      () async throws(SmallErr) -> String in throw SmallErr.boom
    }
    switch f {
    case .success(let s): print("f-ok: \(s)")
    case .failure(let e): print("f-err: \(e)")
    }
    // CHECK-NEXT: f-err: boom

    // g: t2t fed by phi/block-arg operand (R4 — non-FunctionRef rejected).
    let cond = Bool.random()
    let g: Result<String, SmallErr> = await run {
      () async throws(SmallErr) -> String in
        if cond { throw SmallErr.boom } else { throw SmallErr.boom }
    }
    switch g {
    case .success(let s): print("g-ok: \(s)")
    case .failure(let e): print("g-err: \(e)")
    }
    // CHECK-NEXT: g-err: boom

    // h: closure loaded from a stored global (R4 — non-FunctionRef operand).
    let h: Result<String, SmallErr> = await run(StoredClosure.boomClosure)
    switch h {
    case .success(let s): print("h-ok: \(s)")
    case .failure(let e): print("h-err: \(e)")
    }
    // CHECK-NEXT: h-err: boom

    // i: t2t result used across BBs (R5 — cross-BB use rejected).
    var iResult: Result<String, SmallErr>?
    let iClosure: () async throws(SmallErr) -> String = {
      throw SmallErr.boom
    }
    if Bool.random() || true {
      iResult = await run(iClosure)
    }
    switch iResult! {
    case .success(let s): print("i-ok: \(s)")
    case .failure(let e): print("i-err: \(e)")
    }
    // CHECK-NEXT: i-err: boom

    // j: witness-method async typed-throws (R3 via Self-as-polymorphic).
    let j = await invokeWitness(BoomThrower())
    switch j {
    case .success(let s): print("j-ok: \(s)")
    case .failure(let e): print("j-err: \(e)")
    }
    // CHECK-NEXT: j-err: boom

    // k: Thick-self-context async typed-throws (actor isolation).
    let k = await BoomActor().run()
    switch k {
    case .success(let s): print("k-ok: \(s)")
    case .failure(let e): print("k-err: \(e)")
    }
    // CHECK-NEXT: k-err: boom

    // l: large indirect result + indirect error (exercises both
    // hasIndirectSILResults and the typed-error indirect-return path).
    let l: Result<LargeResult, LargeErr> = await run {
      () async throws(LargeErr) -> LargeResult in
        throw LargeErr(tag: 99, pad: (0, 0, 0, 0))
    }
    switch l {
    case .success(let r): print("l-ok: tag=\(r.tag)")
    case .failure(let e): print("l-err: tag=\(e.tag)")
    }
    // CHECK-NEXT: l-err: tag=99

    // m: NEGATIVE — async non-throws (no indirect-error slot, no marker
    // attached, must work). Regression guard against spurious padding.
    let m = await runIdent { () async -> String in "no-error" }
    print("m-ok: \(m)")
    // CHECK-NEXT: m-ok: no-error
  }
}
