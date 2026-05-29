// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -I %t %t/test.swift

// Setup: a struct with two C functions imported as a Swift property via
// `swift_name("getter:...")` / `swift_name("setter:...")`. We deliberately
// give both the getter AND the setter multiple declarations so the redecl
// chain has a non-canonical member that could end up as the lookup-table
// hit during getImplicitProperty's "find the other accessor" loop.

//--- module.modulemap
module ImplicitPropertyRedecl {
    header "implicit-property-redecl.h"
}

//--- implicit-property-redecl.h
#pragma once

struct Box { int data; };

// Canonical
extern int Box_getValue(const struct Box *self)
    __attribute__((swift_name("getter:Box.value(self:)")));
// Most recent
extern int Box_getValue(const struct Box *self)
    __attribute__((swift_name("getter:Box.value(self:)")));

// Canonical
extern void Box_setValue(struct Box *self, int v)
    __attribute__((swift_name("setter:Box.value(self:_:)")));
// Most recent
extern void Box_setValue(struct Box *self, int v)
    __attribute__((swift_name("setter:Box.value(self:_:)")));

//--- test.swift
import ImplicitPropertyRedecl

func test() -> CInt {
    var b = Box(data: 0)
    b.value = 42
    return b.value
}
