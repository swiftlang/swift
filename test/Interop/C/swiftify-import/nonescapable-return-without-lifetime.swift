// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -I %t -plugin-path %swift-plugin-dir -strict-memory-safety -enable-experimental-feature Lifetimes %t/test.swift \
// RUN:    -Rmacro-expansions -verify -verify-additional-file %t%{fs-sep}test.h

//--- module.modulemap
module Test {
    header "test.h"
}

//--- test.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))

struct __attribute__((swift_attr("~Escapable"))) NonescapableStruct {};
// expected-note@+3{{'nonescapableReturnNoLifetime' declared here}}
// expected-error@+2{{a function with a ~Escapable result requires '@_lifetime(...)'}}
// expected-warning@+1{{the returned type 'struct NonescapableStruct' is annotated as non-escapable; its lifetime dependencies must be annotated}}
struct NonescapableStruct nonescapableReturnNoLifetime(int len, int * __counted_by(len) p);

//--- test.swift
import Test

func test(_ p: UnsafeMutablePointer<CInt>, _ len: CInt) {
    let _ = unsafe nonescapableReturnNoLifetime(len, p)
}

func test(_ p: UnsafeMutableBufferPointer<CInt>) -> NonescapableStruct {
    // expected-error@+2{{missing argument for parameter #2 in call}}
    // expected-error@+1{{cannot convert value of type 'UnsafeMutableBufferPointer<CInt>' (aka 'UnsafeMutableBufferPointer<Int32>') to expected argument type 'Int32'}}
    let ret = unsafe nonescapableReturnNoLifetime(p)
    // expected-error@+1{{cannot convert return expression of type 'NonescapableStruct.Type' to return type 'NonescapableStruct'}}
    return NonescapableStruct
}
