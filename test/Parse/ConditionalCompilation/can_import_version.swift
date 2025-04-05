// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/textual)
// RUN: %empty-directory(%t/binary)

// RUN: echo "public func foo() {}" > %t/Foo.swift
// RUN: %target-swift-frontend -emit-module %t/Foo.swift -module-name Foo -swift-version 5 -disable-implicit-concurrency-module-import -user-module-version 113.330.1.2.3 -emit-module-interface-path %t/textual/Foo.swiftinterface -enable-library-evolution -emit-module-path %t/binary/Foo.swiftmodule

// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/textual
// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/binary

import Foo

func canImportVersioned() {
#if canImport(Foo, _version: 0)
  let majorZero = 1 // expected-warning {{initialization of immutable value 'majorZero' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _version: 112)
  let majorSmaller = 1 // expected-warning {{initialization of immutable value 'majorSmaller' was never used; consider replacing with assignment to '_' or removing it}}
#endif
  
#if canImport(Foo, _version: 113)
  let majorEqual = 1 // expected-warning {{initialization of immutable value 'majorEqual' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _version: 114)
  let majorLarger = 1
#endif
  
#if canImport(Foo, _version: 113.329)
  let minorSmaller = 1 // expected-warning {{initialization of immutable value 'minorSmaller' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _version: 113.330)
  let minorEqual = 1 // expected-warning {{initialization of immutable value 'minorEqual' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _version: 113.331)
  let minorLarger = 1
#endif
  
#if canImport(Foo, _version: 113.330.0)
  let patchSmaller = 1 // expected-warning {{initialization of immutable value 'patchSmaller' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _version: 113.330.1)
  let patchEqual = 1 // expected-warning {{initialization of immutable value 'patchEqual' was never used; consider replacing with assignment to '_' or removing it}}
#endif
  
#if canImport(Foo, _version: 113.330.2)
  let patchLarger = 1
#endif

#if canImport(Foo, _version: 113.330.1.1)
  let buildSmaller = 1 // expected-warning {{initialization of immutable value 'buildSmaller' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _version: 113.330.1.2)
  let buildEqual = 1 // expected-warning {{initialization of immutable value 'buildEqual' was never used; consider replacing with assignment to '_' or removing it}}
#endif
  
#if canImport(Foo, _version: 113.330.1.3)
  let buildLarger = 1
#endif
  
#if canImport(Foo, _version: 113.330.1.2.0) // expected-warning {{trailing components of version '113.330.1.2' are ignored}}
  let extraComponent = 1 // expected-warning {{initialization of immutable value 'extraComponent' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _underlyingVersion: 113.33) // expected-warning {{cannot find user version number for Clang module 'Foo'; version number ignored}}
  // TODO(ParserValidation): expected-warning@-1 *{{cannot find user version number for Clang module 'Foo'; version number ignored}}
  // Foo is a Swift module with no underlying clang module.
  let underlyingMinorSmaller = 1 // expected-warning {{initialization of immutable value 'underlyingMinorSmaller' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo)
  let noVersion = 1 // expected-warning {{initialization of immutable value 'noVersion' was never used; consider replacing with assignment to '_' or removing it}}
#endif
}

/// Test versions specified as string literals.
func canImportVersionedString() {
#if canImport(Foo, _version: "0")
  let majorZero = 1 // expected-warning {{initialization of immutable value 'majorZero' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _version: "112")
  let majorSmaller = 1 // expected-warning {{initialization of immutable value 'majorSmaller' was never used; consider replacing with assignment to '_' or removing it}}
#endif
  
#if canImport(Foo, _version: "113")
  let majorEqual = 1 // expected-warning {{initialization of immutable value 'majorEqual' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _version: "114")
  let majorLarger = 1
#endif
  
#if canImport(Foo, _version: "113.329")
  let minorSmaller = 1 // expected-warning {{initialization of immutable value 'minorSmaller' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _version: "113.330")
  let minorEqual = 1 // expected-warning {{initialization of immutable value 'minorEqual' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _version: "113.331")
  let minorLarger = 1
#endif
  
#if canImport(Foo, _version: "113.330.0")
  let patchSmaller = 1 // expected-warning {{initialization of immutable value 'patchSmaller' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _version: "113.330.1")
  let patchEqual = 1 // expected-warning {{initialization of immutable value 'patchEqual' was never used; consider replacing with assignment to '_' or removing it}}
#endif
  
#if canImport(Foo, _version: "113.330.2")
  let patchLarger = 1
#endif

#if canImport(Foo, _version: "113.330.1.1")
  let buildSmaller = 1 // expected-warning {{initialization of immutable value 'buildSmaller' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _version: "113.330.1.2")
  let buildEqual = 1 // expected-warning {{initialization of immutable value 'buildEqual' was never used; consider replacing with assignment to '_' or removing it}}
#endif
  
#if canImport(Foo, _version: "113.330.1.3")
  let buildLarger = 1
#endif

#if canImport(Foo, _version: "113.330.1.2.0") // expected-warning {{trailing components of version '113.330.1.2' are ignored}}
  let extraComponent = 1 // expected-warning {{initialization of immutable value 'extraComponent' was never used; consider replacing with assignment to '_' or removing it}}
#endif

#if canImport(Foo, _underlyingVersion: "113.33") // expected-warning {{cannot find user version number for Clang module 'Foo'; version number ignored}}
  // TODO(ParserValidation): expected-warning@-1 *{{cannot find user version number for Clang module 'Foo'; version number ignored}}
  // Foo is a Swift module with no underlying clang module.
  let underlyingMinorSmaller = 1 // expected-warning {{initialization of immutable value 'underlyingMinorSmaller' was never used; consider replacing with assignment to '_' or removing it}}
#endif
}
