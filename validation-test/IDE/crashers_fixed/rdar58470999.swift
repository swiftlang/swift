// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

protocol P {
  func foo<T: P>(arg: T)
}

func foo(x: P) {
  x.foo(arg: #^COMPLETE^#)
}
