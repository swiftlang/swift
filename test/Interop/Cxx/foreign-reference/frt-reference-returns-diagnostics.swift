// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default \
// RUN:   -I %S%{fs-sep}Inputs \
// RUN:   -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}frt-reference-returns.h

import FRTReferenceReturns

func testNoAnnotations() {
  let _ = NoAnnotations.getRefCountedByRef()
  // expected-warning@-1 {{cannot infer ownership of foreign reference value returned by 'getRefCountedByRef()'}}
  let _ = NoAnnotations.createRefCountedByRef()
  // expected-warning@-1 {{cannot infer ownership of foreign reference value returned by 'createRefCountedByRef()'}}
  let _ = NoAnnotations.copyRefCountedByRef()
  // expected-warning@-1 {{cannot infer ownership of foreign reference value returned by 'copyRefCountedByRef()'}}
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

func testImmortal() {
  let i = createImmortalIntBox()
  let _ = i.builderPattern()

  let d = createDerivedImmortalIntBox()
  let _ = d.builderPattern()
}
