// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/textual)
// RUN: %empty-directory(%t/binary)
// RUN: %empty-directory(%t/module-cache)

// RUN: echo "public func foo() {}" > %t/Foo.swift
// RUN: %target-swift-frontend -emit-module %t/Foo.swift -module-name Foo -swift-version 5 -disable-implicit-concurrency-module-import -user-module-version 113.330 -emit-module-interface-path %t/textual/Foo.swiftinterface -enable-library-evolution -emit-module-path %t/binary/Foo.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/Foo.swift -module-name Bar -swift-version 5 -disable-implicit-concurrency-module-import -emit-module-interface-path %t/textual/Bar.swiftinterface -enable-library-evolution -emit-module-path %t/binary/Bar.swiftmodule

// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/textual
// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/binary

import Foo
import Bar

#if canImport(Bar, _version: 113.331) // expected-warning {{cannot find user version number for Swift module 'Bar'; version number ignored}}
#endif

#if canImport(Bar, _version: 2) // expected-warning {{cannot find user version number for Swift module 'Bar'; version number ignored}}
#endif

func canImportVersioned() {
#if canImport(Foo, _version: 113.331)
  let a = 1
#endif

#if canImport(Foo, _version: 113.3000)
  let b = 1
#endif

#if canImport(Foo, _version: 114)
  let c = 1
#endif

#if canImport(Foo, _version: 4)
  let d = 1 // expected-warning {{initialization of immutable value 'd' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _version: 113.33)
  let e = 1 // expected-warning {{initialization of immutable value 'e' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _underlyingVersion: 113.33)
  let ee = 1
#endif

#if canImport(Foo, _version: 113.329)
  let f = 1 // expected-warning {{initialization of immutable value 'f' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _version: 113.330)
  let g = 1 // expected-warning {{initialization of immutable value 'g' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo)
  let h = 1 // expected-warning {{initialization of immutable value 'h' was never used; consider replacing with assignment to '_' or removing it}}
#endif
}
