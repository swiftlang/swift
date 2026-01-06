// This uses '-primary-file' to ensure we're conservative with lazy SIL emission.
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -suppress-warnings -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name unmapped -primary-file %s %S/Inputs/unmapped_secondary.swift | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s %S/Inputs/unmapped_secondary.swift

// This test is exclusively for AST that we should never profile, as there is
// no interesting user-written code.

// CHECK-NOT: increment_profiler_counter
// CHECK-NOT: sil_coverage_map

struct S {
  // Don't profile the implicit accessor, or the implicit constructor.
  var x: Int
}

// Don't profile any synthesized codable methods.
struct R : Codable {
  var x: String
  var y: Int
}

struct Q {
  // Don't profile the backing initializer.
  @Wrapper
  var x: Int
}

// Don't profile the implicit rawValue.
enum E : Int {
  case a
}

// Don't profile the backing initializers of the property wrapper.
@available(*, unavailable)
func hasExternalPropertyWrapper(@WrapperWithProjectedValue x: Int) {}

// We don't profile unavailable functions, as they don't provide useful coverage
// info.

@available(*, unavailable)
public func unavailableFunction() -> Int {
  .random() ? 1 : 2
}

struct TypeWithUnavailableMethods {
  @available(*, unavailable)
  func foo() -> Int {
    .random() ? 1 : 2
  }

  @available(*, unavailable)
  var qux: Int {
    .random() ? 1 : 2
  }
}

@available(*, unavailable)
extension TypeWithUnavailableMethods {
  func bar() -> Int {
    .random() ? 1 : 2
  }
  public func baz() -> Int {
    .random() ? 1 : 2
  }
}

@available(*, unavailable)
struct UnavailableType {
  func foo() -> Int { .random() ? 1 : 2 }
  public func bar() -> Int { .random() ? 1 : 2 }

  var qux: Int {
    // The && is here to test autoclosures.
    .random() && .random() ? 1 : 2
  }

  var quux: Int {
    get { .random() ? 1 : 2 }
    set { _ = newValue }
  }

  subscript(_ x: Int) -> Int {
    get { .random() ? 1 : 2 }
    set { quux = newValue }
  }

  func baz(_ x: Int = .random() ? 0 : 1) {
    _ = {
      struct Nested {
        func evenMoreNested() -> () -> Int { { .random() ? 1 : 2 } }
      }
      func nested() -> () -> Int { { .random() ? 1 : 2 } }
    }
  }

  var stored: Int = .random() ? 0 : 1

  var storedClosure: Int = { .random() ? 0 : 1 }()

  @Wrapper
  var wrappered = .random() ? 0 : 1

  @Wrapper(wrappedValue: .random() ? 0 : 1)
  var alsoWrappered: Int
}

@available(*, unavailable)
class UnavailableClass {
  deinit {}
}
