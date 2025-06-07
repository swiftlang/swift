// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/pch)
// RUN: split-file %s %t

// Precompile a header into a separate directory and check that it can be
// loaded via its original header name, even if the header isn't adjacent
// to the PCH. This used to be the default, but was changed in clang 16
// (see `CreateInvocationOptions::ProbePrecompiled`).

// RUN: %target-swift-frontend -emit-pch %t/header.h -o %t/pch/header.h.pch
// RUN: %target-swift-frontend -verify -typecheck %t/use.swift -Xcc -include -Xcc %t/pch/header.h -import-objc-header %t/empty.h

//--- empty.h

//--- header.h
void headerFunc();

//--- use.swift
func useHeaderFunc() {
  headerFunc()
}
