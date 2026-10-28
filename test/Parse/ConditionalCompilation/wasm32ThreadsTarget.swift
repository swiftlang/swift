// RUN: %swift -typecheck %s -verify -target wasm32-unknown-wasip1-threads -parse-stdlib
// RUN: %swift-ide-test -test-input-complete -source-filename=%s -target wasm32-unknown-wasip1-threads

// expected-warning@<unknown> * {{libc not found for 'wasm32-unknown-wasip1-threads'; C stdlib may be unavailable}}

#if _runtime(_multithreaded)
  func underThreads() {
    foo() // expected-error {{cannot find 'foo' in scope}}
  }
#endif
