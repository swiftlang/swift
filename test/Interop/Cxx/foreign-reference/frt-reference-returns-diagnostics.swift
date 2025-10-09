// RUN: rm -rf %t
// RUN: %target-typecheck-verify-swift -I %S%{fs-sep}Inputs -cxx-interoperability-mode=default -enable-experimental-feature WarnUnannotatedReturnOfCxxFrt -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}frt-reference-returns.h

// REQUIRES: swift_feature_WarnUnannotatedReturnOfCxxFrt

import FRTReferenceReturns

func testNoAnnotations() {
  let _ = NoAnnotations.getRefCountedByRef()
  // expected-warning@-1 {{cannot infer the ownership of the returned value, annotate 'getRefCountedByRef()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
  let _ = NoAnnotations.createRefCountedByRef()
  // expected-warning@-1 {{cannot infer the ownership of the returned value, annotate 'createRefCountedByRef()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
  let _ = NoAnnotations.copyRefCountedByRef()
  // expected-warning@-1 {{cannot infer the ownership of the returned value, annotate 'copyRefCountedByRef()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
}

func testAPIAnnotations() {
  let _ = APIAnnotations.createByRef()
  let _ = APIAnnotations.getByRef()
}

func testTypeAnnotation() {
  let _ = TypeAnnotation.getByRef()
  let _ = TypeAnnotation.createByRef()
  let _ = TypeAnnotation.copyByRef()
}

func testBothAnnotations() {
  let _ = BothAnnotations.createByRef() 
  let _ = BothAnnotations.getByRef()
}
