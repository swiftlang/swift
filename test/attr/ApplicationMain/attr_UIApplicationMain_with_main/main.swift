// expected-note{{top-level code defined in this source file}}
// expected-note@-1{{pass '-parse-as-library' to compiler invocation if this is intentional}}
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s %S/delegate.swift

// REQUIRES: objc_interop

hi()
