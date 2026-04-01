// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify %t/test.swift \
// RUN:   -I %t \
// RUN:   -verify-additional-file %t%{fs-sep}header.h

//--- header.h
struct
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:MyRetain")))
__attribute__((swift_attr("release:MyRelease")))
MyFRT {
  int value;
};

void MyRetain(struct MyFRT *x);
void MyRelease(struct MyFRT *x);

struct MyFRT *getConflicting() // #hdrMarker1
    __attribute__((swift_attr("returns_retained")))
    __attribute__((swift_attr("returns_unretained")));

//--- module.modulemap
module TestClangMarker {
  header "header.h"
  export *
}

//--- test.swift
import TestClangMarker

// Marker defined in the C header, referenced from Swift file.
// expected-error@#hdrMarker1 {{cannot be annotated with both}}

// Trigger the diagnostic by using the function.
let _ = getConflicting()
