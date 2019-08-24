// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s


struct S<T> {}
func use<T>(x: (S<T>) -> Void) {}

func test() {
  use { $0 #^COMPLETE^# }
}
