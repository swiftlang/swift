// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil %s

protocol P {
  associatedtype T
  @_borrowed subscript(x: T) -> Int { get }
}

struct S<T> : P {
  subscript(x: T) -> Int { return 0 }
}
