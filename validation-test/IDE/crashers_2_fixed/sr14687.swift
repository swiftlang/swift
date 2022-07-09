// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE

class Foo {
  let searchSubject = Bar<String, #^COMPLETE^#
}
