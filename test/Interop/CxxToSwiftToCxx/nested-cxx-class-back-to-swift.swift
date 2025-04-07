// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/use-cxx-types.swift -module-name UseCxxTy -typecheck -verify -emit-clang-header-path %t/UseCxxTy.h -I %t -enable-experimental-cxx-interop -clang-header-expose-decls=all-public -disable-availability-checking
// RUN: cat %t/header.h >> %t/full-header.h
// RUN: cat %t/UseCxxTy.h >> %t/full-header.h
// RUN: %target-interop-build-clangxx -std=c++20 -c -xc++-header %t/full-header.h -o %t/o.o

//--- header.h

struct Cell { class Visitor {}; };

//--- module.modulemap
module CxxTest {
    header "header.h"
    requires cplusplus
}

//--- use-cxx-types.swift
import CxxTest

public extension Cell.Visitor {
    func visit() {}
}

public func f() -> [Cell.Visitor] {
}
