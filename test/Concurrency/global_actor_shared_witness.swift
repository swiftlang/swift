// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify

// REQUIRES: concurrency

actor MyActor {}

// ==== -----------------------------------------------------------------------
// MARK: Stored 'let' is the canonical safe form, no diagnostic.

@globalActor
struct LetItBe {
  static let shared = MyActor()
}

// ==== -----------------------------------------------------------------------
// MARK: Stored 'var' may be reassigned, warn.

@globalActor
struct VarStored {
  static var shared = MyActor()
  // expected-warning@-1{{GlobalActor witness static property 'shared' may return different actor instances, which would lead to global actor isolation violations}}
  // expected-note@-2{{declare it as 'static let' to guarantee a stable instance}}{{10-13=let}}
  // expected-note@-3{{if this property always returns the same instance, silence the warning with '@diagnose(UnstableGlobalActorShared, as: ignored)'}}{{3-3=@diagnose(UnstableGlobalActorShared, as: ignored) }}
}

// ==== -----------------------------------------------------------------------
// MARK: Computed property may return different instances, warn.

@globalActor
struct VarComputed {
  static var shared: MyActor { MyActor() }
  // expected-warning@-1{{GlobalActor witness static property 'shared' may return different actor instances, which would lead to global actor isolation violations}}
  // expected-note@-2{{declare it as 'static let' to guarantee a stable instance}}
  // expected-note@-3{{if this property always returns the same instance, silence the warning with '@diagnose(UnstableGlobalActorShared, as: ignored)'}}
}

// ==== -----------------------------------------------------------------------
// MARK: Computed property delegating to a stored let; silenced with '@diagnose'.

@globalActor
struct VarComputedCached {
  private static let _shared = MyActor()
  @diagnose(UnstableGlobalActorShared, as: ignored)
  static var shared: MyActor { _shared }
}

// ==== -----------------------------------------------------------------------
// MARK: The original bug from issue #89901: @TaskLocal-backed witness.

@globalActor
actor MyTaskLocalActor {
  @TaskLocal
  static var shared = MyTaskLocalActor()
  // expected-warning@-1{{GlobalActor witness static property 'shared' may return different actor instances, which would lead to global actor isolation violations}}
  // expected-note@-2{{declare it as 'static let' to guarantee a stable instance}}
  // expected-note@-3{{if this property always returns the same instance, silence the warning with '@diagnose(UnstableGlobalActorShared, as: ignored)'}}
}

// ==== -----------------------------------------------------------------------
// MARK: Diagnostic only fires for actual @globalActor types.

// Not @globalActor, no warning even though shape would otherwise trigger.
struct NotGlobalActor {
  static var shared = MyActor()
}
