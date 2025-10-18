// RUN: %target-swift-frontend -emit-sil %s

// Properly make inherited initializers 'async' when needed.
@available(SwiftStdlib 5.5, *)
class Base {
  required init() async { }
}

@available(SwiftStdlib 5.5, *)
class Derived: Base { }


@available(SwiftStdlib 5.5, *)
class Base2 {
  required init() async throws { }
}

@available(SwiftStdlib 5.5, *)
class Derived2: Base2 { }

