// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck %t/test.swift -import-bridging-header %t/test.h -verify -Xcc -Wno-nullability-completeness
// RUN: %target-swift-frontend -typecheck %t/test.swift -import-bridging-header %t/test.h -verify -Xcc -fbounds-safety -Xcc -Wno-nullability-completeness -disable-objc-interop

//--- test.h
#pragma once

#include <ptrcheck.h>

typedef int * int_ptr_t;
typedef int_ptr_t _Nullable nullable_int_ptr_t;

int_ptr_t __single foo(int_ptr_t __single p);
nullable_int_ptr_t __single bar(nullable_int_ptr_t __single p);

//--- test.swift
func call_foo(p: int_ptr_t) {
  foo(p)
}

func call_bar(p: nullable_int_ptr_t) {
  bar(p)
}

// typedefs with these names exist in the clang AST,
// but they should not be visible to the user.

// expected-error@+1{{cannot find type 'int_ptr_t __single' in scope}}
typealias aaa = `int_ptr_t __single`
// expected-error@+1{{cannot find type 'nullable_int_ptr_t __single' in scope}}
typealias bbb = `nullable_int_ptr_t __single`
