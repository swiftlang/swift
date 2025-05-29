// RUN: %swift -typecheck %s -verify -target arm64-apple-none-macho -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target arm64-apple-none-macho

#if !_runtime(_multithreaded)
  func underNoThreads() {
    foo() // expected-error {{cannot find 'foo' in scope}}
  }
#endif
