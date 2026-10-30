// RUN: split-file %s %t
//
// RUN: %target-swift-frontend -typecheck -verify -suppress-remarks -suppress-notes \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -I %t%{fs-sep}Inputs %t%{fs-sep}main.swift \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}objcxx-header.h
//
// REQUIRES: objc_interop

// Verify that ObjCBool type is banned from C++ template parameters.
// Note that the error messages appear in the header file.

//--- Inputs/module.modulemap
module ObjCxxHeader {
  header "objcxx-header.h"
  requires cplusplus, objc
}

//--- Inputs/objcxx-header.h
#pragma once

template <typename T> void takesObjCBool(T) {}
// expected-error@-1 {{could not be converted: ObjCBool}}

template <typename T> void takesConstLRefObjCBool(const T &) {}
// expected-error@-1 {{could not be converted: ObjCBool}}

//--- main.swift
import ObjCxxHeader
import ObjectiveC

takesObjCBool(ObjCBool(true))

takesConstLRefObjCBool(ObjCBool(true))
