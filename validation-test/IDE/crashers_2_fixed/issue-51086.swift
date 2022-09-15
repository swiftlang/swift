// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

// https://github.com/apple/swift/issues/51086

struct S<T> {}
func use<T>(x: (S<T>) -> Void) {}

func test() {
  use { $0 #^COMPLETE^# }
}
