// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify \
// RUN:   -cxx-interoperability-mode=default -disable-availability-checking \
// RUN:   -I %t%{fs-sep}Inputs -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}test.h \
// RUN:   %t%{fs-sep}test.swift

//--- Inputs/module.modulemap
module Test {
    header "test.h"
}

//--- Inputs/test.h
#pragma once
#pragma clang assume_nonnull begin

struct
  __attribute__((swift_attr("import_reference")))
  __attribute__((swift_attr("retain:nonexistent")))
  __attribute__((swift_attr("release:nonexistent")))
NonExistent { int value; };
// expected-error@-1 {{cannot find retain function 'nonexistent' for reference type 'NonExistent'}}
// expected-error@-2 {{cannot find release function 'nonexistent' for reference type 'NonExistent'}}

struct
  __attribute__((swift_attr("import_reference")))
NoRetainRelease { int value; };
// expected-error@-1 {{reference type 'NoRetainRelease' must have exactly one 'retain:' Swift attribute}}
// expected-error@-2 {{reference type 'NoRetainRelease' must have exactly one 'release:' Swift attribute}}

struct
  __attribute__((swift_attr("import_reference")))
  __attribute__((swift_attr("retain:badRetain")))
  __attribute__((swift_attr("release:badRelease")))
BadRetainRelease { int value; };
// expected-error@-1 {{release function 'badRelease' must have exactly one argument of type 'BadRetainRelease'}}
// expected-error@-2 {{retain function 'badRetain' must return an integer, its parameter type, or 'void'}}

float badRetain(struct BadRetainRelease *v);
void badRelease(struct BadRetainRelease *v, int i);

#pragma clang assume_nonnull end

//--- test.swift
import Test

public func test(x: NonExistent) { }
public func test(x: NoRetainRelease) { }
public func test(x: BadRetainRelease) { }
