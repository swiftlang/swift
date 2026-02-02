// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t/main.swift -I %t -verify-additional-file %t/cmodule.h

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

//--- cmodule.h
#import <CoreGraphics/CoreGraphics.h>
#define intLiteralCGFloat ((CGFloat)0)
// expected-note@-1 {{invalid numeric literal}}
// expected-note@-2 {{macro 'intLiteralCGFloat' unavailable (cannot import)}}
#define floatLiteralCGFloat ((CGFloat)0.0)
// expected-note@-1 {{invalid numeric literal}}
// expected-note@-2 {{macro 'floatLiteralCGFloat' unavailable (cannot import)}}

//--- module.modulemap
module CModule [system] {
  header "cmodule.h"
  export *
}

//--- main.swift
import CModule

// Make sure we don't crash when attempting to import these.
_ = intLiteralCGFloat // expected-error {{cannot find 'intLiteralCGFloat' in scope}}
_ = floatLiteralCGFloat // expected-error {{cannot find 'floatLiteralCGFloat' in scope}}
