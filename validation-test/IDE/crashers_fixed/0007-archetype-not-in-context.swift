// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s

class D<X, Y>() {}

class C<T> {
  func f<U>() -> D<U, T> {}
  func g() {
    f#^A^#
  }
}
