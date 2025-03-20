// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -swift-version 5 -enable-experimental-feature ExecutionAttribute -enable-experimental-feature AsyncCallerExecution:adoption
// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -swift-version 6 -enable-experimental-feature ExecutionAttribute -enable-experimental-feature AsyncCallerExecution:adoption

// REQUIRES: swift_feature_ExecutionAttribute
// REQUIRES: swift_feature_AsyncCallerExecution

struct G<T> {
  init(_: T) {}
}

@execution(concurrent) func globalAsyncF() async {}

// MARK: Functions
do {
  func syncF() {}
  @execution(concurrent) func executionConcurrentAsyncF() async {}
  @execution(caller) func executionCallerAsyncF() async {}
  @MainActor func mainActorAsyncF() async {}
  func isolatedParamAsyncF(
    isolation: isolated (any Actor)? = #isolation
  ) async {}

  // expected-warning@+1:20 {{feature 'AsyncCallerExecution' will cause nonisolated async local function 'asyncF' to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{3-3=@execution(concurrent) }}{{none}}
  nonisolated func asyncF() async {}

  struct S {
    init(sync: ()) {}
    @execution(concurrent) init(executionAsync: ()) async {}
    @MainActor init(mainActorAsync: ()) async {}
    // expected-warning@+1:5 {{feature 'AsyncCallerExecution' will cause nonisolated async initializer 'init' to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{5-5=@execution(concurrent) }}{{none}}
    init(async: ()) async {}

    func syncF() {}
    @execution(concurrent) func executionAsyncF() async {}
    @MainActor func mainActorAsyncF() async {}
    // expected-warning@+2:17 {{feature 'AsyncCallerExecution' will cause nonisolated async instance method 'asyncF' to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{-1:5-5=@execution(concurrent) }}{{none}}
    nonisolated
    public func asyncF() async {}
  }

  protocol P {
    // FIXME: Not diagnosed
    func asyncF() async
  }
}

// MARK: Storage
do {
  struct S {
    var storedVar: Int
    let storedLet: Int

    var syncS: Int { get {} set {} }
    subscript(syncS _: Int) -> Int { get {} }

    @execution(concurrent) var executionAsyncS: Int { get async {} }
    @execution(concurrent) subscript(executionAsyncS _: Int) -> Int { get async {} }

    @MainActor var mainActorAsyncS: Int { get async {} }
    @MainActor subscript(mainActorAsyncS _: Int) -> Int { get async {} }

    // expected-warning@+2:7 {{feature 'AsyncCallerExecution' will cause nonisolated async getter for property 'asyncS' to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{-1:5-5=@execution(concurrent) }}{{none}}
    var asyncS: Int {
      get async {}
    }
    // expected-warning@+2:7 {{feature 'AsyncCallerExecution' will cause nonisolated async getter for subscript 'subscript' to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{-1:5-5=@execution(concurrent) }}{{none}}
    subscript(asyncS _: Int) -> Int {
      get async throws {}
    }
  }

  protocol P {
    var syncS: Int { get }
    subscript(syncS _: Int) -> Int { get }

    @execution(concurrent) var executionAsyncS: Int { get async }
    @execution(concurrent) subscript(executionAsyncS _: Int) -> Int { get async }

    @MainActor var mainActorAsyncS: Int { get async }
    @MainActor subscript(mainActorAsyncS _: Int) -> Int { get async }

    // expected-warning@+1:23 {{feature 'AsyncCallerExecution' will cause nonisolated async getter for property 'asyncS' to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{5-5=@execution(concurrent) }}{{none}}
    var asyncS: Int { get async }
    // FIXME: Not diagnosed
    subscript(asyncS _: Int) -> Int { get async }
  }
}

// MARK: Parameters
do {
  enum E {
    case esac(
      sync: () -> Void,
      executionAsync: @execution(concurrent) () async -> Void,
      mainActorAsync: @MainActor () async -> Void,
      // expected-warning@+1:14 {{feature 'AsyncCallerExecution' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{14-14=@execution(concurrent) }}{{none}}
      async: () async -> Void,
      // expected-warning@+1:26 {{feature 'AsyncCallerExecution' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{26-26=@execution(concurrent) }}{{none}}
      structuralAsync: G<() async -> Void>
    )
  }

  struct S {
    subscript(
      sync: () -> Void,
      executionAsync: @execution(concurrent) () async -> Void,
      mainActorAsync: @MainActor () async -> Void,
      // expected-warning@+1:14 {{feature 'AsyncCallerExecution' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{14-14=@execution(concurrent) }}{{none}}
      async: () async -> Void,
      // expected-warning@+1:26 {{feature 'AsyncCallerExecution' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{26-26=@execution(concurrent) }}{{none}}
      structuralAsync: G<() async -> Void>
    ) -> Int {
      0
    }
  }

  func foo(
    sync: () -> Void,
    executionAsync: @execution(concurrent) () async -> Void,
    mainActorAsync: @MainActor () async -> Void,
    // expected-warning@+1:12 {{feature 'AsyncCallerExecution' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{12-12=@execution(concurrent) }}{{none}}
    async: () async -> Void,
    // expected-warning@+1:24 {{feature 'AsyncCallerExecution' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{24-24=@execution(concurrent) }}{{none}}
    structuralAsync: G<() async -> Void>
  ) {}

  let _ = { (
    sync: () -> Void,
    executionAsync: @execution(concurrent) () async -> Void,
    mainActorAsync: @MainActor () async -> Void,
    // expected-warning@+1:12 {{feature 'AsyncCallerExecution' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{12-12=@execution(concurrent) }}{{none}}
    async: () async -> Void,
    // expected-warning@+1:24 {{feature 'AsyncCallerExecution' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{24-24=@execution(concurrent) }}{{none}}
    structuralAsync: G<() async -> Void>
  ) in
  }
}

// MARK: Generic constraints
do {
  struct G<T> {
    struct Sync where T == () -> Void {}
    struct ExecutionAsync where T == @execution(concurrent) () async -> Void {}
    struct MainActorAsync where T == @MainActor () async -> Void {}
    // expected-warning@+1:29 {{feature 'AsyncCallerExecution' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{29-29=@execution(concurrent) }}{{none}}
    struct Async where T == () async -> Void {}
    // expected-warning@+1:41 {{feature 'AsyncCallerExecution' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{41-41=@execution(concurrent) }}{{none}}
    struct StructuralAsync where T == G<() async -> Void> {}
  }
}

// MARK: Variables
do {
  let _: () -> Void
  let _: @execution(concurrent) () async -> Void
  let _: @MainActor () async -> Void
  // expected-warning@+1:10 {{feature 'AsyncCallerExecution' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{10-10=@execution(concurrent) }}{{none}}
  let _: () async -> Void
  // expected-warning@+1:12 {{feature 'AsyncCallerExecution' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{12-12=@execution(concurrent) }}{{none}}
  let _: G<() async -> Void>
}

// MARK: Casts
do {
  let anything: Any

  let _ = anything as? () -> Void
  let _ = anything as? @execution(concurrent) () async -> Void
  let _ = anything as? @MainActor () async -> Void
  // expected-warning@+1:24 {{feature 'AsyncCallerExecution' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{24-24=@execution(concurrent) }}{{none}}
  let _ = anything as? () async -> Void
  // expected-warning@+1:26 {{feature 'AsyncCallerExecution' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{26-26=@execution(concurrent) }}{{none}}
  let _ = anything as? G<() async -> Void>
}

// MARK: Closures
do {
  nonisolated
  func nonisolatedF() {
    let _ = { () -> Void in }

    let _ = { @execution(concurrent) () async -> Void in }
    let _ = { @MainActor () async -> Void in }

    // expected-warning@+1:13 {{feature 'AsyncCallerExecution' will cause nonisolated async closure to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{15-15=@execution(concurrent) }}{{none}}
    let _ = { () async -> Void in }

    func takesInts(_: Int...) {}

    // expected-warning@+1:13 {{feature 'AsyncCallerExecution' will cause nonisolated async closure to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{14-14= @execution(concurrent) in }}{{none}}
    let _ = {await globalAsyncF()}
    // expected-warning@+1:13 {{feature 'AsyncCallerExecution' will cause nonisolated async closure to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{14-14= @execution(concurrent) in }}{{none}}
    let _ = {
      await globalAsyncF()
    }
    // expected-warning@+1:13 {{feature 'AsyncCallerExecution' will cause nonisolated async closure to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{14-14= @execution(concurrent) in }}{{none}}
    let _ = {
      await globalAsyncF()
      takesInts($0, $1, $2)
    }
    // expected-warning@+1:13 {{feature 'AsyncCallerExecution' will cause nonisolated async closure to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{25-25=@execution(concurrent) }}{{none}}
    let _ = { @Sendable in
      await globalAsyncF()
    }
    // expected-warning@+2:18 {{feature 'AsyncCallerExecution' will cause nonisolated async function type to be treated as specified to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{18-18=@execution(concurrent) }}{{none}}
    // expected-warning@+1:45 {{feature 'AsyncCallerExecution' will cause nonisolated async closure to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{47-47=@execution(concurrent) }}{{none}}
    var closure: (Int, Int) async -> Void = { a, b in
      await globalAsyncF()
    }
    // expected-warning@+1:15 {{feature 'AsyncCallerExecution' will cause nonisolated async closure to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{+1:7-7=@execution(concurrent) }}{{none}}
    closure = {
      a, b async in
      await globalAsyncF()
    }
    // expected-warning@+1:15 {{feature 'AsyncCallerExecution' will cause nonisolated async closure to run on the caller's actor; use @execution(concurrent) to preserve behavior}}{{17-17=@execution(concurrent) }}{{none}}
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

