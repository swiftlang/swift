// RUN: %target-swift-frontend -target %target-future-triple -parse-as-library -emit-silgen -verify %s


@globalActor final actor FirstActor {
  static let shared = FirstActor()
}

struct AS: ~Copyable {
  @FirstActor deinit {} // expected-error {{only classes and actors can have isolated deinit}}
}

@FirstActor
struct BS: ~Copyable {
  isolated deinit {} // expected-error {{only classes and actors can have isolated deinit}}
}

struct CS: ~Copyable {
  isolated deinit {} // expected-error {{only classes and actors can have isolated deinit}}
}

struct DS: ~Copyable {
  nonisolated deinit {}
}

struct ES: ~Copyable {
  @FirstActor isolated deinit {} // expected-error 2 {{only classes and actors can have isolated deinit}}
}

enum AE: ~Copyable {
  // expected-error@+1 {{deinitializers are not yet supported on noncopyable enums}}
  @FirstActor deinit {} // expected-error {{only classes and actors can have isolated deinit}}
}

@FirstActor
enum BE: ~Copyable {
  case dummy
  // expected-error@+1 {{deinitializers are not yet supported on noncopyable enums}}
  isolated deinit {} // expected-error {{only classes and actors can have isolated deinit}}
}

enum CE: ~Copyable {
  case dummy
  // expected-error@+1 {{deinitializers are not yet supported on noncopyable enums}}
  isolated deinit {} // expected-error {{only classes and actors can have isolated deinit}}
}

enum DE: ~Copyable {
  case dummy
  // expected-error@+1 {{deinitializers are not yet supported on noncopyable enums}}
  nonisolated deinit {}
}

enum EE: ~Copyable {
  case dummy
  // expected-error@+1 {{deinitializers are not yet supported on noncopyable enums}}
  @FirstActor isolated deinit {} // expected-error 2 {{only classes and actors can have isolated deinit}}
}
