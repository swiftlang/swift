// REQUIRES: swift_feature_StabilizedSafeInteropWrappers

// Tests that we don't try to import modules that don't work well with C++ interop (when enabled)

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// Case 1: a.swift calls A1.B1.foo with C++ interop
// RUN: not %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -cxx-interoperability-mode=default -I %t%{fs-sep}Inputs %t/a.swift -dump-source-file-imports 2>&1 | %FileCheck --check-prefixes CHECK-A,CHECK-A-CXX %s
// RUN:     %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -cxx-interoperability-mode=default -I %t%{fs-sep}Inputs %t/a.swift -verify -verify-additional-prefix cxx- -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}B1.h -verify-ignore-macro-note

// Case 2: a.swift calls A1.B1.foo without C++ interop
// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t%{fs-sep}Inputs %t/a.swift -dump-source-file-imports 2>&1 | %FileCheck --check-prefixes CHECK-A,CHECK-A-C %s
// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t%{fs-sep}Inputs %t/a.swift -verify

// Case 3: b.swift calls A2.B2.foo with C++ interop
// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -cxx-interoperability-mode=default -I %t%{fs-sep}Inputs %t/b.swift -dump-source-file-imports 2>&1 | %FileCheck --check-prefixes CHECK-B,CHECK-B-CXX %s
// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -cxx-interoperability-mode=default -I %t%{fs-sep}Inputs %t/b.swift -verify

// Case 4: b.swift calls A2.B2.foo without C++ interop
// RUN: not %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t%{fs-sep}Inputs %t/b.swift -dump-source-file-imports 2>&1 | %FileCheck --check-prefix CHECK-B %s
// RUN:     %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t%{fs-sep}Inputs %t/b.swift -verify -verify-additional-prefix c- -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}B2.h -verify-ignore-macro-note

// CHECK-A:      imports for {{.*}}a.swift:
// CHECK-A-NEXT: 	Swift
// CHECK-A-CXX-NEXT: 	CxxShim
// CHECK-A-CXX-NEXT: 	Cxx
// CHECK-A-NEXT: 	_StringProcessing
// CHECK-A-NEXT: 	_SwiftConcurrencyShims
// CHECK-A-NEXT: 	_Concurrency
// CHECK-A-NEXT: 	B1
// CHECK-A-NEXT: 	A1
// CHECK-A-NEXT: imports for A1.foo:

// CHECK-A-NEXT: imports for @__swiftmacro{{.*}}foo{{.*}}_SwiftifyImport{{.*}}.swift:
// CHECK-A-NEXT: Swift
// CHECK-A-C-NEXT: C1
// CHECK-A-NEXT: B1
// CHECK-A-CXX-NEXT: CxxShim
// CHECK-A-CXX-NEXT: Cxx
// CHECK-A-NEXT: _StringProcessing
// CHECK-A-NEXT: _SwiftConcurrencyShims
// CHECK-A-NEXT: _Concurrency

// CHECK-B:      imports for {{.*}}b.swift:
// CHECK-B-NEXT: 	Swift
// CHECK-B-CXX-NEXT: 	CxxShim
// CHECK-B-CXX-NEXT: 	Cxx
// CHECK-B-NEXT: 	_StringProcessing
// CHECK-B-NEXT: 	_SwiftConcurrencyShims
// CHECK-B-NEXT: 	_Concurrency
// CHECK-B-NEXT: 	B2
// CHECK-B-NEXT: 	A2
// CHECK-B-NEXT: imports for A2.bar:

// CHECK-B-NEXT: imports for @__swiftmacro{{.*}}bar{{.*}}_SwiftifyImport{{.*}}.swift:
// CHECK-B-NEXT: Swift
// CHECK-B-CXX-NEXT: C2
// CHECK-B-NEXT: B2
// CHECK-B-CXX-NEXT: CxxShim
// CHECK-B-CXX-NEXT: Cxx
// CHECK-B-NEXT: _StringProcessing
// CHECK-B-NEXT: _SwiftConcurrencyShims
// CHECK-B-NEXT: _Concurrency


//--- Inputs/module.modulemap
  module A1 {
    explicit module B1 {
      header "B1.h"
      explicit module C1 {
        header "C1.h"
        requires !cplusplus
      }
    }
  }
  module A2 {
    explicit module B2 {
      header "B2.h"
      explicit module C2 {
        header "C2.h"
        requires cplusplus
      }
    }
  }

//--- Inputs/B1.h
  #pragma once
  
  #include "C1.h"
  #define __sized_by(s) __attribute__((__sized_by__(s)))
  
  // foo causes an error with cxx-interop
  c1_t foo(void * _Nonnull __sized_by(size), int size);
  /*
  expected-cxx-note@-2{{'foo' declared here}}
  expected-cxx-expansion@-3:54{{
    expected-cxx-error@2:110{{cannot find type 'c1_t' in scope}}
  }}
  */

//--- Inputs/B2.h
  #pragma once
  
  #include "C2.h"
  #define __sized_by(s) __attribute__((__sized_by__(s)))
  
  // bar causes an error without cxx-interop
  c2_t bar(void * _Nonnull __sized_by(size), int size);
  /*
  expected-c-note@-2{{'bar' declared here}}
  expected-c-expansion@-3:54{{
    expected-c-error@2:110{{cannot find type 'c2_t' in scope}}
  }}
  */

//--- Inputs/C1.h
  #pragma once
  
  typedef int c1_t;

//--- Inputs/C2.h
  #pragma once
  
  typedef int c2_t;

//--- a.swift
  import A1.B1
  
  public func callUnsafe(_ p: UnsafeMutableRawPointer) {
    let _ = foo(p, 13)
  }
  public func callSafe(_ p: UnsafeMutableRawBufferPointer) {
    let _ = foo(p) // expected-cxx-error{{cannot convert value of type 'UnsafeMutableRawBufferPointer' to expected argument type 'UnsafeMutableRawPointer'}}
                   // expected-cxx-error@-1{{missing argument}}
  }
  
//--- b.swift
  import A2.B2

  public func callUnsafe(_ p: UnsafeMutableRawPointer) {
    let _ = bar(p, 13)
  }
  public func callSafe(_ p: UnsafeMutableRawBufferPointer) {
    let _ = bar(p) // expected-c-error{{cannot convert value of type 'UnsafeMutableRawBufferPointer' to expected argument type 'UnsafeMutableRawPointer'}}
                   // expected-c-error@-1{{missing argument}}

  }
