// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE

func foo(closure: (String) -> Void) -> String? {
  return nil
}

func test() {
  if let key = foo(closure: { str in str.suffix(2) == #^COMPLETE^#
