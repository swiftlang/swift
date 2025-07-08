// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -swift-version 5 -enable-upcoming-feature NonisolatedNonsendingByDefault:migrate
// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -swift-version 6 -enable-upcoming-feature NonisolatedNonsendingByDefault:migrate

// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

struct G<T> {
  init(_: T) {}
}

@concurrent func globalAsyncF() async {}

// MARK: Functions
do {
  func syncF() {}
  @concurrent func concurrentAsyncF() async {}
  nonisolated(nonsending) func nonisolatedNonsendingAsyncF() async {}
  @MainActor func mainActorAsyncF() async {}
  func isolatedParamAsyncF(
    isolation: isolated (any Actor)? = #isolation
  ) async {}

  // expected-warning@+1:20 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async local function 'asyncF' to run on the caller's actor; use '@concurrent' to preserve behavior}}{{3-3=@concurrent }}{{none}}
  nonisolated func asyncF() async {}

  struct S {
    init(sync: ()) {}
    @concurrent init(concurrentAsync: ()) async {}
    nonisolated(nonsending) init(nonisolatedNonsendingAsync: ()) async {}
    @MainActor init(mainActorAsync: ()) async {}
    // expected-warning@+1:5 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async initializer 'init' to run on the caller's actor; use '@concurrent' to preserve behavior}}{{5-5=@concurrent }}{{none}}
    init(async: ()) async {}

    func syncF() {}
    @concurrent func concurrentAsyncF() async {}
    nonisolated(nonsending) func nonisolatedNonsendingAsyncF() async {}
    @MainActor func mainActorAsyncF() async {}
    // expected-warning@+2:17 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async instance method 'asyncF' to run on the caller's actor; use '@concurrent' to preserve behavior}}{{-1:5-5=@concurrent }}{{none}}
    nonisolated
    public func asyncF() async {}
  }

  protocol P {
    init(sync: ())
    @concurrent init(concurrentAsync: ()) async
    nonisolated(nonsending) init(nonisolatedNonsendingAsync: ()) async
    @MainActor init(mainActorAsync: ()) async
    // expected-warning@+1:5 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async initializer 'init' to run on the caller's actor; use '@concurrent' to preserve behavior}}{{5-5=@concurrent }}{{none}}
    init(async: ()) async

    func syncF()
    @concurrent func concurrentAsyncF() async
    nonisolated(nonsending) func nonisolatedNonsendingAsyncF() async
    @MainActor func mainActorAsyncF() async
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async instance method 'asyncF' to run on the caller's actor; use '@concurrent' to preserve behavior}}{{5-5=@concurrent }}{{none}}
    func asyncF() async
  }
}
protocol Functions {}
extension Functions {
  init(sync: ()) {}
  @concurrent init(concurrentAsync: ()) async {}
  nonisolated(nonsending) init(nonisolatedNonsendingAsync: ()) async {}
  @MainActor init(mainActorAsync: ()) async {}
  // expected-warning@+1:3 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async initializer 'init' to run on the caller's actor; use '@concurrent' to preserve behavior}}{{3-3=@concurrent }}{{none}}
  init(async: ()) async {}

  func syncF() {}
  @concurrent func concurrentAsyncF() async {}
  nonisolated(nonsending) func nonisolatedNonsendingAsyncF() async {}
  @MainActor func mainActorAsyncF() async {}
  // expected-warning@+1:8 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async instance method 'asyncF' to run on the caller's actor; use '@concurrent' to preserve behavior}}{{3-3=@concurrent }}{{none}}
  func asyncF() async {}
}

// MARK: Storage
do {
  struct S {
    var storedVar: Int
    let storedLet: Int

    var syncS: Int { get {} set {} }
    subscript(syncS _: Int) -> Int { get {} }

    @concurrent var concurrentAsyncS: Int { get async {} }
    @concurrent subscript(concurrentAsyncS _: Int) -> Int { get async {} }

    nonisolated(nonsending) var nonisolatedNonsendingAsyncS: Int { get async {} }
    nonisolated(nonsending) subscript(nonisolatedNonsendingAsyncS _: Int) -> Int { get async {} }

    @MainActor var mainActorAsyncS: Int { get async {} }
    @MainActor subscript(mainActorAsyncS _: Int) -> Int { get async {} }

    // expected-warning@+2:7 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async getter for property 'asyncS' to run on the caller's actor; use '@concurrent' to preserve behavior}}{{-1:5-5=@concurrent }}{{none}}
    var asyncS: Int {
      get async {}
    }
    // expected-warning@+2:7 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async getter for subscript 'subscript' to run on the caller's actor; use '@concurrent' to preserve behavior}}{{-1:5-5=@concurrent }}{{none}}
    subscript(asyncS _: Int) -> Int {
      get async throws {}
    }
  }

  protocol P {
    var syncS: Int { get }
    subscript(syncS _: Int) -> Int { get }

    @concurrent var concurrentAsyncS: Int { get async }
    @concurrent subscript(concurrentAsyncS _: Int) -> Int { get async }

    nonisolated(nonsending) var nonisolatedNonsendingAsyncS: Int { get async }
    nonisolated(nonsending) subscript(nonisolatedNonsendingAsyncS _: Int) -> Int { get async }

    @MainActor var mainActorAsyncS: Int { get async }
    @MainActor subscript(mainActorAsyncS _: Int) -> Int { get async }

    // expected-warning@+1:23 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async getter for property 'asyncS' to run on the caller's actor; use '@concurrent' to preserve behavior}}{{5-5=@concurrent }}{{none}}
    var asyncS: Int { get async }
    // expected-warning@+1:39 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async getter for subscript 'subscript' to run on the caller's actor; use '@concurrent' to preserve behavior}}{{5-5=@concurrent }}{{none}}
    subscript(asyncS _: Int) -> Int { get async }
  }
}
protocol Storage {}
extension Storage {
  var syncS: Int { get {} set {} }
  subscript(syncS _: Int) -> Int { get {} }

  @concurrent var concurrentAsyncS: Int { get async {} }
  @concurrent subscript(concurrentAsyncS _: Int) -> Int { get async {} }

  nonisolated(nonsending) var nonisolatedNonsendingAsyncS: Int { get async {} }
  nonisolated(nonsending) subscript(nonisolatedNonsendingAsyncS _: Int) -> Int { get async {} }

  @MainActor var mainActorAsyncS: Int { get async {} }
  @MainActor subscript(mainActorAsyncS _: Int) -> Int { get async {} }

  // expected-warning@+2:5 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async getter for property 'asyncS' to run on the caller's actor; use '@concurrent' to preserve behavior}}{{-1:3-3=@concurrent }}{{none}}
  var asyncS: Int {
    get async {}
  }
  // expected-warning@+2:5 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async getter for subscript 'subscript' to run on the caller's actor; use '@concurrent' to preserve behavior}}{{-1:3-3=@concurrent }}{{none}}
  subscript(asyncS _: Int) -> Int {
    get async throws {}
  }
}


// MARK: Parameters
do {
  enum E {
    case esac(
      sync: () -> Void,
      concurrentAsync: @concurrent () async -> Void,
      nonisolatedNonsendingAsync: nonisolated(nonsending) () async -> Void,
      mainActorAsync: @MainActor () async -> Void,
      // expected-warning@+1:14 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use '@concurrent' to preserve behavior}}{{14-14=@concurrent }}{{none}}
      async: () async -> Void,
      // expected-warning@+1:26 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use '@concurrent' to preserve behavior}}{{26-26=@concurrent }}{{none}}
      structuralAsync: G<() async -> Void>
    )
  }

  struct S {
    subscript(
      sync: () -> Void,
      concurrentAsync: @concurrent () async -> Void,
      nonisolatedNonsendingAsync: nonisolated(nonsending) () async -> Void,
      mainActorAsync: @MainActor () async -> Void,
      // expected-warning@+1:14 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use '@concurrent' to preserve behavior}}{{14-14=@concurrent }}{{none}}
      async: () async -> Void,
      // expected-warning@+1:26 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use '@concurrent' to preserve behavior}}{{26-26=@concurrent }}{{none}}
      structuralAsync: G<() async -> Void>
    ) -> Int {
      0
    }
  }

  func foo(
    sync: () -> Void,
    concurrentAsync: @concurrent () async -> Void,
    nonisolatedNonsendingAsync: nonisolated(nonsending) () async -> Void,
    mainActorAsync: @MainActor () async -> Void,
    // expected-warning@+1:12 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    async: () async -> Void,
    // expected-warning@+1:24 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use '@concurrent' to preserve behavior}}{{24-24=@concurrent }}{{none}}
    structuralAsync: G<() async -> Void>
  ) {}

  let _ = { (
    sync: () -> Void,
    concurrentAsync: @concurrent () async -> Void,
    nonisolatedNonsendingAsync: nonisolated(nonsending) () async -> Void,
    mainActorAsync: @MainActor () async -> Void,
    // expected-warning@+1:12 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    async: () async -> Void,
    // expected-warning@+1:24 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use '@concurrent' to preserve behavior}}{{24-24=@concurrent }}{{none}}
    structuralAsync: G<() async -> Void>
  ) in
  }
}

// MARK: Generic constraints
do {
  struct G<T> {
    struct Sync where T == () -> Void {}
    struct ConcurrentAsync where T == @concurrent () async -> Void {}
    struct NonisolatedNonsendingAsync where T == nonisolated(nonsending) () async -> Void {}
    struct MainActorAsync where T == @MainActor () async -> Void {}
    // expected-warning@+1:29 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use '@concurrent' to preserve behavior}}{{29-29=@concurrent }}{{none}}
    struct Async where T == () async -> Void {}
    // expected-warning@+1:41 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use '@concurrent' to preserve behavior}}{{41-41=@concurrent }}{{none}}
    struct StructuralAsync where T == G<() async -> Void> {}
  }
}

// MARK: Variables
do {
  let _: () -> Void
  let _: @concurrent () async -> Void
  let _: nonisolated(nonsending) () async -> Void
  let _: @MainActor () async -> Void
  // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use '@concurrent' to preserve behavior}}{{10-10=@concurrent }}{{none}}
  let _: () async -> Void
  // expected-warning@+1:12 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
  let _: G<() async -> Void>
}

// MARK: Casts
do {
  let anything: Any

  let _ = anything as? () -> Void
  let _ = anything as? @concurrent () async -> Void
  let _ = anything as? nonisolated(nonsending) () async -> Void
  let _ = anything as? @MainActor () async -> Void
  // expected-warning@+1:24 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use '@concurrent' to preserve behavior}}{{24-24=@concurrent }}{{none}}
  let _ = anything as? () async -> Void
  // expected-warning@+1:26 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use '@concurrent' to preserve behavior}}{{26-26=@concurrent }}{{none}}
  let _ = anything as? G<() async -> Void>
}

// MARK: Closures
do {
  @concurrent
  func asyncOnly(_: Int, _: Int) async {}
  @concurrent
  @Sendable
  func asyncThrows(_: Int, _: Int) async throws {}

  nonisolated
  func test(_: @concurrent (Int, Int) async throws -> Void) {}

  nonisolated
  func nonisolatedF() {
    let x = 0

    let _ = { () -> Void in }

    let _ = { @concurrent () async -> Void in }
    let _ = { @MainActor () async -> Void in }

    // Make sure all of these case compile with the fix-its applied.
    //
    // Cases where we add inferred effects to ensure it compiles with '@concurrent'.

    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{34-34=async throws }}{{none}}
    test { (a: consuming Int, b) in                               try await asyncThrows(a, b) }
    test { @concurrent (a: consuming Int, b) async throws in      try await asyncThrows(a, b) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{29-29=async throws }}{{none}}
    test { (a: Int, b: Int) in                                    try await asyncThrows(a, b) }
    test { @concurrent (a: Int, b: Int) async throws in           try await asyncThrows(a, b) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{22-22=@concurrent }}{{39-39=async throws }}{{none}}
    test { @Sendable (a: Int, b: Int) in                          try await asyncThrows(a, b) }
    test { @Sendable @concurrent (a: Int, b: Int) async throws in try await asyncThrows(a, b) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{24-24=async throws }}{{none}}
    test { (a: Int, b) in                                         try await asyncThrows(a, b) }
    test { @concurrent (a: Int, b) async throws in                try await asyncThrows(a, b) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{19-19=async throws }}{{none}}
    test { (a, b) in                                              try await asyncThrows(a, b) }
    test { @concurrent (a, b) async throws in                     try await asyncThrows(a, b) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{19-19=async }}{{none}}
    test { (a, b) throws -> Void in                               try await asyncThrows(a, b) }
    test { @concurrent (a, b) async throws -> Void in             try await asyncThrows(a, b) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{23-23=async }}{{none}}
    test { [x] (a, b) throws -> Void in                           try await asyncThrows(a, b + x) }
    test { @concurrent [x] (a, b) async throws -> Void in         try await asyncThrows(a, b + x) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{19-19=async throws }}{{none}}
    test { (a, b) -> Void in                                      try await asyncThrows(a, b) }
    test { @concurrent (a, b) async throws -> Void in             try await asyncThrows(a, b) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{23-23=async throws }}{{none}}
    test { [x] (a, b) -> Void in                                  try await asyncThrows(a, b + x) }
    test { @concurrent [x] (a, b) async throws -> Void in         try await asyncThrows(a, b + x) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{17-17=async throws }}{{none}}
    test { a, b -> Void in                                        try await asyncThrows(a, b) }
    test { @concurrent a, b async throws -> Void in               try await asyncThrows(a, b) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{17-17=async throws }}{{none}}
    test { _, _ -> Void in                                        try await asyncThrows(1, 2) }
    test { @concurrent _, _ async throws -> Void in               try await asyncThrows(1, 2) }

    // Cases that will compile with '@concurrent' as is.

    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    test { (a: Int, b: Int) throws in                             try await asyncThrows(a, b) }
    test { @concurrent (a: Int, b: Int) throws in                 try await asyncThrows(a, b) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    test { (a, b) throws in                                       try await asyncThrows(a, b) }
    test { @concurrent (a, b) throws in                           try await asyncThrows(a, b) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    test { (_, _) throws in                                       try await asyncThrows(1, 2) }
    test { @concurrent (_, _) throws in                           try await asyncThrows(1, 2) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    test { (a, b) async throws in                                 try await asyncThrows(a, b) }
    test { @concurrent (a, b) async throws in                     try await asyncThrows(a, b) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    test { (a, b) async in                                        await asyncOnly(a, b) }
    test { @concurrent (a, b) async in                            await asyncOnly(a, b) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    test { a, b async -> Void in                                  await asyncOnly(a, b) }
    test { @concurrent a, b async -> Void in                      await asyncOnly(a, b) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    test { a, b in                                                try await asyncThrows(a, b) }
    test { @concurrent a, b in                                    try await asyncThrows(a, b) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    test { a, b throws in                                         try await asyncThrows(a, b) }
    test { @concurrent a, b throws in                             try await asyncThrows(a, b) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    test { a, b async in                                          await asyncOnly(a, b) }
    test { @concurrent a, b async in                              await asyncOnly(a, b) }

    // No space after 'in' necessary.
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{11-11= @concurrent in}}{{none}}
    test {                                                        try await asyncThrows($0, $1) }
    // No space after 'in' necessary.
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{11-11= @concurrent in}}{{none}}
    test { try await asyncThrows($0, $1) }
    // No space after 'in' necessary.
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{11-11= @concurrent in}}{{none}}
    test {
      try await asyncThrows($0, $1)
    }
    // Add a space after in.
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{11-11= @concurrent in }}{{none}}
    test {try await asyncThrows($0, $1)}

    test { @concurrent in                                         try await asyncThrows($0, $1) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    test { [x] in                                                 try await asyncThrows($0, $1 + x) }
    test { @concurrent [x] in                                     try await asyncThrows($0, $1 + x) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{22-22=@concurrent }}{{none}}
    test { @Sendable [x] in                                       try await asyncThrows($0, $1 + x) }
    test { @Sendable @concurrent [x] in                           try await asyncThrows($0, $1 + x) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    test { [x] a, b in                                            try await asyncThrows(a, b + x) }
    test { @concurrent [x] a, b in                                try await asyncThrows(a, b + x) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    test { [x] a, b async in                                      await asyncOnly(a, b + x) }
    test { @concurrent [x] a, b async in                          await asyncOnly(a, b + x) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    test { [x] a, b throws in                                     try await asyncThrows(a, b + x) }
    test { @concurrent [x] a, b throws in                         try await asyncThrows(a, b + x) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    test { [x] (a, b) in                                          try await asyncThrows(a, b + x) }
    test { @concurrent [x] (a, b) in                              try await asyncThrows(a, b + x) }
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use '@concurrent' to preserve behavior}}{{12-12=@concurrent }}{{none}}
    test { [x] (a: Int, _: Int) in                                try await asyncThrows(a, x) }
    test { @concurrent [x] (a: Int, _: Int) in                    try await asyncThrows(a, x) }
  }

  @MainActor
  func mainActorF() {
    let _ = { () async -> Void in }
    let _ = {
      await globalAsyncF()
    }
  }
}

// The compiler used to mark autoclosure in `MyThing` reference as "non-implicit"
// which caused a crash in migration mode because it assumes that autoclosures
// are always implicit.
do {
  struct Other {
    init(test: Int) {}
  }

  struct S<T> {
    typealias MyThing = Other
    var value: T
  }

  func test(c: S<Int?>) {
    _ = c.value.map {
      type(of: c).MyThing(test: $0) // Ok (used to crash due to autoclosure use)
    }
  }
}

// @_inheritActorContext prevents `nonisolated(nonsending)` inference.
do {
  func testInherit1(@_inheritActorContext _: @Sendable () async -> Void) {}
  func testInherit2(@_inheritActorContext(always) _: (@Sendable () async -> Void)?) {}
}
