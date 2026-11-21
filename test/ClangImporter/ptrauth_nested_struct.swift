// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift -typecheck -verify -verify-ignore-unrelated -target arm64e-apple-macos13.0 -I %t %t/test.swift

// REQUIRES: CPU=arm64e
// REQUIRES: OS=macosx

// Structs that nest structs with address-discriminated ptrauth-qualified
// fields are imported. Nesting other non-trivial fields is still rejected.

//--- module.modulemap
module PtrAuthNested {
  header "header.h"
  export *
}

//--- header.h
#include <ptrauth.h>

typedef struct {
  const int *__ptrauth(ptrauth_key_process_independent_data, 1, 1234) p;
} Inner;

typedef struct {
  Inner inner;
} Outer;

typedef struct {
  __strong id obj;
} StrongInner;

typedef struct {
  StrongInner inner;
} NestsStrong;

//--- test.swift
import PtrAuthNested

func useOuter(_ o: Outer) {}
func useNestsStrong(_ n: NestsStrong) {} // expected-error {{cannot find type 'NestsStrong' in scope}}
