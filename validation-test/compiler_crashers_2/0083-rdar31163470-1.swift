// RUN: not --crash %target-swift-frontend -primary-file %s -emit-ir
// REQUIRES: asserts

struct First<T> {}
struct Second<T> {}

struct Node<T> {
  func create<U>() where T == First<U> { }
  func create<U>() where T == Second<U> { }
}
