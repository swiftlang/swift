// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -I %t -cxx-interoperability-mode=default %t/test.swift

// A typedef's underlying type sugars back to the typedef's enclosing record,
// std::ratio<1>::type (`typedef ratio<num,den> type;` in libc++ <ratio>).

//--- module.modulemap
module RecursiveRecordTypedef {
    header "recursive-record-typedef.h"
    requires cplusplus
}

//--- recursive-record-typedef.h
#pragma once

namespace M {
  template <int N>
  struct S {
    // Self-referential: the underlying type *is* the enclosing class template
    // specialization S<N>. Importing this typedef therefore recurses into
    // importing S<N>, which in turn iterates its members, including this
    // typedef.
    typedef S<N> type;
  };
}

// Top-level typedef chain so that importing `Trigger` drives
// SwiftTypeConverter through TypedefType('M::S<1>::type') and into the
// recursive record import.
//
// Mirrors how libc++'s std::chrono::duration<long long>::period is encountered
// while importing std::chrono::seconds.
typedef M::S<1>::type Trigger;

//--- test.swift
import RecursiveRecordTypedef

// Force import of `Trigger` -- before the fix, this aborted while importing
// `M::S<1>::type`.
let _: Trigger? = nil
