// RUN: %swift-ide-test --code-completion --source-filename %s --code-completion-token=COMPLETE

// https://github.com/apple/swift/issues/57105

Foo.#^COMPLETE^#bar ?? baz(_) = baba