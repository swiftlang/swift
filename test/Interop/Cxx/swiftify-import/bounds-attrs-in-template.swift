// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=default -enable-experimental-feature SafeInteropWrappers %t/template.swift -dump-macro-expansions -emit-ir -o %t/out -verify
// RUN: %target-swift-ide-test -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=default -enable-experimental-feature SafeInteropWrappers -print-module -module-to-print=Template -source-filename=x | %FileCheck %s

// CHECK: func cb_template<T>(_ p: UnsafePointer<T>, _ size: Int{{.*}}) -> UnsafePointer<T>
// CHECK: func eb_template<T>(_ p: UnsafePointer<T>, _ end: UnsafePointer<T>) -> UnsafePointer<T>
// CHECK: func s_template<T>(_ p: UnsafePointer<T>) -> UnsafePointer<T>
// CHECK: func ui_template<T>(_ p: UnsafePointer<T>) -> UnsafePointer<T>

//--- Inputs/module.modulemap
module Template {
    header "template.h"
    requires cplusplus
}

//--- Inputs/template.h
#include <ptrcheck.h>
#include <lifetimebound.h>

template <class T>
inline const T* __counted_by(size) cb_template(const T* __counted_by(size) p __noescape, int size) { return p; }

template <class T>
inline const T* __ended_by(end) eb_template(const T* __ended_by(end) p __noescape, const T* end) { return p; }

template <class T>
inline const T* __single s_template(const T* __single p) { return p; }

template <class T>
inline const T* __unsafe_indexable ui_template(const T* __unsafe_indexable p) { return p; }

// FIXME: parse null_terminated in templated contexts
// template <class T>
// inline const T* __null_terminated nt_template(const T* __null_terminated p) {}

//--- template.swift
import Template

// make sure the original functions are still available when parsing bounds attributes
func testOriginalCB(p: UnsafePointer<CInt>, len: CInt) {
  let _ = cb_template(p, len)
}
func testOriginalEB(p: UnsafePointer<CInt>, end: UnsafePointer<CInt>) {
  let _ = eb_template(p, end)
}
func testOriginalS(s: UnsafePointer<CInt>) {
  let _ = s_template(s)
}
func testOriginalUI(s: UnsafePointer<CInt>) {
  let _ = ui_template(s)
}

 // FIXME: generate safe overloads for templated functions (rdar://151481042)
func testSafeOverloadCB(s: Span<CInt>) {
  // expected-error@+3{{generic parameter 'T' could not be inferred}}
  // expected-error@+2{{cannot convert value of type 'Span<CInt>'}}
  // expected-error@+1{{missing argument for parameter #2 in call}}
  cb_template(s)
}
func testSafeOverloadEB(s: Span<CInt>) {
  // expected-error@+3{{generic parameter 'T' could not be inferred}}
  // expected-error@+2{{cannot convert value of type 'Span<CInt>'}}
  // expected-error@+1{{missing argument for parameter #2 in call}}
  eb_template(s)
}
