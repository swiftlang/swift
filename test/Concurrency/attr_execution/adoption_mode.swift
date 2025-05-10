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
  @concurrent func executionConcurrentAsyncF() async {}
  nonisolated(nonsending) func executionCallerAsyncF() async {}
  @MainActor func mainActorAsyncF() async {}
  func isolatedParamAsyncF(
    isolation: isolated (any Actor)? = #isolation
  ) async {}

  // expected-warning@+1:20 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async local function 'asyncF' to run on the caller's actor; use @concurrent to preserve behavior}}{{3-3=@concurrent }}{{none}}
  nonisolated func asyncF() async {}

  struct S {
    init(sync: ()) {}
    @concurrent init(executionAsync: ()) async {}
    @MainActor init(mainActorAsync: ()) async {}
    // expected-warning@+1:5 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async initializer 'init' to run on the caller's actor; use @concurrent to preserve behavior}}{{5-5=@concurrent }}{{none}}
    init(async: ()) async {}

    func syncF() {}
    @concurrent func executionAsyncF() async {}
    @MainActor func mainActorAsyncF() async {}
    // expected-warning@+2:17 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async instance method 'asyncF' to run on the caller's actor; use @concurrent to preserve behavior}}{{-1:5-5=@concurrent }}{{none}}
    nonisolated
    public func asyncF() async {}
  }

  protocol P {
    init(sync: ())
    @concurrent init(executionAsync: ()) async
    @MainActor init(mainActorAsync: ()) async
    // expected-warning@+1:5 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async initializer 'init' to run on the caller's actor; use @concurrent to preserve behavior}}{{5-5=@concurrent }}{{none}}
    init(async: ()) async

    func syncF()
    @concurrent func executionAsyncF() async
    @MainActor func mainActorAsyncF() async
    // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async instance method 'asyncF' to run on the caller's actor; use @concurrent to preserve behavior}}{{5-5=@concurrent }}{{none}}
    func asyncF() async
  }
}
protocol Functions {}
extension Functions {
  init(sync: ()) {}
  @concurrent init(executionAsync: ()) async {}
  @MainActor init(mainActorAsync: ()) async {}
  // expected-warning@+1:3 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async initializer 'init' to run on the caller's actor; use @concurrent to preserve behavior}}{{3-3=@concurrent }}{{none}}
  init(async: ()) async {}

  func syncF() {}
  @concurrent func executionAsyncF() async {}
  @MainActor func mainActorAsyncF() async {}
  // expected-warning@+1:8 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async instance method 'asyncF' to run on the caller's actor; use @concurrent to preserve behavior}}{{3-3=@concurrent }}{{none}}
  func asyncF() async {}
}

// MARK: Storage
do {
  struct S {
    var storedVar: Int
    let storedLet: Int

    var syncS: Int { get {} set {} }
    subscript(syncS _: Int) -> Int { get {} }

    @concurrent var executionAsyncS: Int { get async {} }
    @concurrent subscript(executionAsyncS _: Int) -> Int { get async {} }

    @MainActor var mainActorAsyncS: Int { get async {} }
    @MainActor subscript(mainActorAsyncS _: Int) -> Int { get async {} }

    // expected-warning@+2:7 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async getter for property 'asyncS' to run on the caller's actor; use @concurrent to preserve behavior}}{{-1:5-5=@concurrent }}{{none}}
    var asyncS: Int {
      get async {}
    }
    // expected-warning@+2:7 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async getter for subscript 'subscript' to run on the caller's actor; use @concurrent to preserve behavior}}{{-1:5-5=@concurrent }}{{none}}
    subscript(asyncS _: Int) -> Int {
      get async throws {}
    }
  }

  protocol P {
    var syncS: Int { get }
    subscript(syncS _: Int) -> Int { get }

    @concurrent var executionAsyncS: Int { get async }
    @concurrent subscript(executionAsyncS _: Int) -> Int { get async }

    @MainActor var mainActorAsyncS: Int { get async }
    @MainActor subscript(mainActorAsyncS _: Int) -> Int { get async }

    // expected-warning@+1:23 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async getter for property 'asyncS' to run on the caller's actor; use @concurrent to preserve behavior}}{{5-5=@concurrent }}{{none}}
    var asyncS: Int { get async }
    // expected-warning@+1:39 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async getter for subscript 'subscript' to run on the caller's actor; use @concurrent to preserve behavior}}{{5-5=@concurrent }}{{none}}
    subscript(asyncS _: Int) -> Int { get async }
  }
}
protocol Storage {}
extension Storage {
  var syncS: Int { get {} set {} }
  subscript(syncS _: Int) -> Int { get {} }

  @concurrent var executionAsyncS: Int { get async {} }
  @concurrent subscript(executionAsyncS _: Int) -> Int { get async {} }

  @MainActor var mainActorAsyncS: Int { get async {} }
  @MainActor subscript(mainActorAsyncS _: Int) -> Int { get async {} }

  // expected-warning@+2:5 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async getter for property 'asyncS' to run on the caller's actor; use @concurrent to preserve behavior}}{{-1:3-3=@concurrent }}{{none}}
  var asyncS: Int {
    get async {}
  }
  // expected-warning@+2:5 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async getter for subscript 'subscript' to run on the caller's actor; use @concurrent to preserve behavior}}{{-1:3-3=@concurrent }}{{none}}
  subscript(asyncS _: Int) -> Int {
    get async throws {}
  }
}


// MARK: Parameters
do {
  enum E {
    case esac(
      sync: () -> Void,
      executionAsync: @concurrent () async -> Void,
      mainActorAsync: @MainActor () async -> Void,
      // expected-warning@+1:14 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @concurrent to preserve behavior}}{{14-14=@concurrent }}{{none}}
      async: () async -> Void,
      // expected-warning@+1:26 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @concurrent to preserve behavior}}{{26-26=@concurrent }}{{none}}
      structuralAsync: G<() async -> Void>
    )
  }

  struct S {
    subscript(
      sync: () -> Void,
      executionAsync: @concurrent () async -> Void,
      mainActorAsync: @MainActor () async -> Void,
      // expected-warning@+1:14 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @concurrent to preserve behavior}}{{14-14=@concurrent }}{{none}}
      async: () async -> Void,
      // expected-warning@+1:26 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @concurrent to preserve behavior}}{{26-26=@concurrent }}{{none}}
      structuralAsync: G<() async -> Void>
    ) -> Int {
      0
    }
  }

  func foo(
    sync: () -> Void,
    executionAsync: @concurrent () async -> Void,
    mainActorAsync: @MainActor () async -> Void,
    // expected-warning@+1:12 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @concurrent to preserve behavior}}{{12-12=@concurrent }}{{none}}
    async: () async -> Void,
    // expected-warning@+1:24 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @concurrent to preserve behavior}}{{24-24=@concurrent }}{{none}}
    structuralAsync: G<() async -> Void>
  ) {}

  let _ = { (
    sync: () -> Void,
    executionAsync: @concurrent () async -> Void,
    mainActorAsync: @MainActor () async -> Void,
    // expected-warning@+1:12 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @concurrent to preserve behavior}}{{12-12=@concurrent }}{{none}}
    async: () async -> Void,
    // expected-warning@+1:24 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @concurrent to preserve behavior}}{{24-24=@concurrent }}{{none}}
    structuralAsync: G<() async -> Void>
  ) in
  }
}

// MARK: Generic constraints
do {
  struct G<T> {
    struct Sync where T == () -> Void {}
    struct ExecutionAsync where T == @concurrent () async -> Void {}
    struct MainActorAsync where T == @MainActor () async -> Void {}
    // expected-warning@+1:29 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @concurrent to preserve behavior}}{{29-29=@concurrent }}{{none}}
    struct Async where T == () async -> Void {}
    // expected-warning@+1:41 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @concurrent to preserve behavior}}{{41-41=@concurrent }}{{none}}
    struct StructuralAsync where T == G<() async -> Void> {}
  }
}

// MARK: Variables
do {
  let _: () -> Void
  let _: @concurrent () async -> Void
  let _: @MainActor () async -> Void
  // expected-warning@+1:10 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @concurrent to preserve behavior}}{{10-10=@concurrent }}{{none}}
  let _: () async -> Void
  // expected-warning@+1:12 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @concurrent to preserve behavior}}{{12-12=@concurrent }}{{none}}
  let _: G<() async -> Void>
}

// MARK: Casts
do {
  let anything: Any

  let _ = anything as? () -> Void
  let _ = anything as? @concurrent () async -> Void
  let _ = anything as? @MainActor () async -> Void
  // expected-warning@+1:24 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @concurrent to preserve behavior}}{{24-24=@concurrent }}{{none}}
  let _ = anything as? () async -> Void
  // expected-warning@+1:26 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @concurrent to preserve behavior}}{{26-26=@concurrent }}{{none}}
  let _ = anything as? G<() async -> Void>
}

// MARK: Closures
do {
  nonisolated
  func nonisolatedF() {
    let _ = { () -> Void in }

    let _ = { @concurrent () async -> Void in }
    let _ = { @MainActor () async -> Void in }

    // expected-warning@+1:13 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use @concurrent to preserve behavior}}{{15-15=@concurrent }}{{none}}
    let _ = { () async -> Void in }

    func takesInts(_: Int...) {}

    // expected-warning@+1:13 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use @concurrent to preserve behavior}}{{14-14= @concurrent in }}{{none}}
    let _ = {await globalAsyncF()}
    // expected-warning@+1:13 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use @concurrent to preserve behavior}}{{14-14= @concurrent in }}{{none}}
    let _ = {
      await globalAsyncF()
    }
    // expected-warning@+1:13 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use @concurrent to preserve behavior}}{{14-14= @concurrent in }}{{none}}
    let _ = {
      await globalAsyncF()
      takesInts($0, $1, $2)
    }
    // expected-warning@+1:13 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use @concurrent to preserve behavior}}{{25-25=@concurrent }}{{none}}
    let _ = { @Sendable in
      await globalAsyncF()
    }
    // expected-warning@+2:18 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @concurrent to preserve behavior}}{{18-18=@concurrent }}{{none}}
    // expected-warning@+1:45 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use @concurrent to preserve behavior}}{{47-47=@concurrent }}{{none}}
    var closure: (Int, Int) async -> Void = { a, b in
      await globalAsyncF()
    }
    // expected-warning@+1:15 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use @concurrent to preserve behavior}}{{+1:7-7=@concurrent }}{{none}}
    closure = {
      a, b async in
      await globalAsyncF()
    }
    // expected-warning@+1:15 {{feature 'NonisolatedNonsendingByDefault' will cause nonisolated async closure to run on the caller's actor; use @concurrent to preserve behavior}}{{17-17=@concurrent }}{{none}}
    closure = { (a, b) in
      await globalAsyncF()
    }

    let _ = closure
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
