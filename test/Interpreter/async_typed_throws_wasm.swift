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

// For shape h: closure loaded from storage rather than a literal.
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

// For shape q: witness-method x LargeErr.
protocol LargeAsync {
  func boom() async throws(LargeErr) -> String
}
struct LargeBoom: LargeAsync {
  func boom() async throws(LargeErr) -> String {
    throw LargeErr(tag: 11, pad: (0, 0, 0, 0))
  }
}
func invokeLarge<T: LargeAsync>(_ x: T) async -> Result<String, LargeErr> {
  do { return .success(try await x.boom()) }
  catch { return .failure(error) }
}

// For shape r: actor cross-isolation with large typed error.
actor LargeActor {
  func bang() async throws(LargeErr) -> String {
    throw LargeErr(tag: 13, pad: (0, 0, 0, 0))
  }
}

// For shape s: @MainActor + async typed-throws with large error.
@MainActor
func mainLargeErr() async throws(LargeErr) -> String {
  throw LargeErr(tag: 17, pad: (0, 0, 0, 0))
}

// For shape t: replica of the stdlib Result.init(catching:) initializer,
// with Failure inferred as `any Error` from an untyped-throws closure.
enum MyResult<Success: ~Copyable, Failure: Error>: ~Copyable {
  case success(Success)
  case failure(Failure)
}
extension MyResult: Copyable where Success: Copyable {}

extension MyResult where Success: ~Copyable {
  @_alwaysEmitIntoClient
  nonisolated(nonsending) init(
    catching body: nonisolated(nonsending) () async throws(Failure) -> Success
  ) async {
    do {
      self = .success(try await body())
    } catch {
      self = .failure(error)
    }
  }
}

@main
struct Main {
  static func main() async {
    // a: untyped throws (baseline repro).
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

    // c: typed throws, large error struct (indirect error).
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

    // e: non-throwing closure (no indirect-error slot).
    let e = await runIdent { () async -> String in "noerror" }
    print("e-ok: \(e)")
    // CHECK-NEXT: e-ok: noerror

    // f: generic typed-throws via an explicit type argument.
    let f: Result<String, SmallErr> = await run {
      () async throws(SmallErr) -> String in throw SmallErr.boom
    }
    switch f {
    case .success(let s): print("f-ok: \(s)")
    case .failure(let e): print("f-err: \(e)")
    }
    // CHECK-NEXT: f-err: boom

    // g: closure value from a phi/block argument.
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

    // h: closure loaded from a stored global.
    let h: Result<String, SmallErr> = await run(StoredClosure.boomClosure)
    switch h {
    case .success(let s): print("h-ok: \(s)")
    case .failure(let e): print("h-err: \(e)")
    }
    // CHECK-NEXT: h-err: boom

    // i: closure result used across basic blocks.
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

    // j: witness-method typed-throws.
    let j = await invokeWitness(BoomThrower())
    switch j {
    case .success(let s): print("j-ok: \(s)")
    case .failure(let e): print("j-err: \(e)")
    }
    // CHECK-NEXT: j-err: boom

    // k: thick self-context (actor isolation).
    let k = await BoomActor().run()
    switch k {
    case .success(let s): print("k-ok: \(s)")
    case .failure(let e): print("k-err: \(e)")
    }
    // CHECK-NEXT: k-err: boom

    // l: large indirect result + indirect error.
    let l: Result<LargeResult, LargeErr> = await run {
      () async throws(LargeErr) -> LargeResult in
        throw LargeErr(tag: 99, pad: (0, 0, 0, 0))
    }
    switch l {
    case .success(let r): print("l-ok: tag=\(r.tag)")
    case .failure(let e): print("l-err: tag=\(e.tag)")
    }
    // CHECK-NEXT: l-err: tag=99

    // m: negative case: async non-throwing (no indirect-error slot).
    let m = await runIdent { () async -> String in "no-error" }
    print("m-ok: \(m)")
    // CHECK-NEXT: m-ok: no-error

    // n: control-flow-driven capture; success and error paths must not
    // conflate the context and error slots.
    let capture = Int.random(in: 1...10)
    let nClosure: () async throws(SmallErr) -> String = {
      if capture > 0 { return "ok-\(capture)" } else { throw SmallErr.boom }
    }
    let nResult = await run(nClosure)
    switch nResult {
    case .success(let s): print("n-ok: \(s.hasPrefix("ok-"))")
    case .failure(let err): print("n-err: \(err)")
    }
    // CHECK-NEXT: n-ok: true

    // o: typed throws returning Void (indirect error, Void result).
    let oResult: Result<Void, SmallErr> = await run {
      () async throws(SmallErr) -> Void in throw SmallErr.boom
    }
    switch oResult {
    case .success: print("o-ok")
    case .failure(let err): print("o-err: \(err)")
    }
    // CHECK-NEXT: o-err: boom

    // p: multiple indirect results + typed error.
    let pResult: Result<(LargeResult, LargeResult), LargeErr> = await run {
      () async throws(LargeErr) -> (LargeResult, LargeResult) in
        throw LargeErr(tag: 7, pad: (0, 0, 0, 0))
    }
    switch pResult {
    case .success(let pair): print("p-ok: \(pair.0.tag)")
    case .failure(let err): print("p-err: tag=\(err.tag)")
    }
    // CHECK-NEXT: p-err: tag=7

    // q: witness-method + large error; Self+WT trail the
    // [indirect error, swiftself] pair.
    let qResult = await invokeLarge(LargeBoom())
    switch qResult {
    case .success(let s): print("q-ok: \(s)")
    case .failure(let err): print("q-err: tag=\(err.tag)")
    }
    // CHECK-NEXT: q-err: tag=11

    // r: actor cross-isolation with a large typed error (hop thunk).
    let rResult: Result<String, LargeErr>
    do { rResult = .success(try await LargeActor().bang()) }
    catch { rResult = .failure(error) }
    switch rResult {
    case .success(let s): print("r-ok: \(s)")
    case .failure(let err): print("r-err: tag=\(err.tag)")
    }
    // CHECK-NEXT: r-err: tag=13

    // s: @MainActor hop with a large typed error.
    let sResult: Result<String, LargeErr>
    do { sResult = .success(try await mainLargeErr()) }
    catch { sResult = .failure(error) }
    switch sResult {
    case .success(let s): print("s-ok: \(s)")
    case .failure(let err): print("s-err: tag=\(err.tag)")
    }
    // CHECK-NEXT: s-err: tag=17

    // t: Result.init(catching:) replica; Failure inferred as any Error.
    func asyncThrowing() async throws -> String {
      throw SmallErr.boom
    }
    let t = await MyResult { try await asyncThrowing() }
    switch t {
    case .success(let s): print("t-ok: \(s)")
    case .failure(let e): print("t-err: \(e)")
    }
    // CHECK-NEXT: t-err: boom
  }
}
