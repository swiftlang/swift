// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-sil %t/valid.swift -I %t
// RUN: %target-swift-frontend -typecheck -verify %t/invalid.swift -I %t -disable-typo-correction

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

//--- cmodule.h
#include <stdbool.h>

typedef _Bool UnderscoreBoolAlias;
#define usesUnderscoreBoolAlias ((UnderscoreBoolAlias)0)
#define usesUnderscoreBool ((_Bool)0)

typedef bool BoolAlias;
#define usesBoolAlias ((BoolAlias)0)
#define usesBool ((bool)0)

//--- module.modulemap
module CModule {
  header "cmodule.h"
  requires objc
  export *
}

//--- valid.swift
import CModule

// Make sure we can import these constants.
print(usesUnderscoreBoolAlias)
print(usesBoolAlias)

//--- invalid.swift
import CModule

// FIXME: These ought to work too
print(usesUnderscoreBool) // expected-error {{cannot find 'usesUnderscoreBool' in scope}}
print(usesBool) // expected-error {{cannot find 'usesBool' in scope}}
