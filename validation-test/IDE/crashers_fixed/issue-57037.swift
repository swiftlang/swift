// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE

// https://github.com/apple/swift/issues/57037

class Foo {
  let searchSubject = Bar<String, #^COMPLETE^#
}
