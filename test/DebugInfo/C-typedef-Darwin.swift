// RUN: %target-swift-frontend -emit-ir %s -g -I %S/Inputs | %FileCheck %s
// REQUIRES: OS=macosx

import Darwin

func use<T>(_ t: T) {}

let blah = size_t(1024)
use(blah)
// CHECK: !DIDerivedType(tag: DW_TAG_typedef,
// CHECK-SAME:           scope: ![[DARWIN_MODULE:[0-9]+]],

// size_t is defined in clang, originally by <stddef.h>, and later split out to
// <__stddef_size_t.h>. Depending on the state of clang's builtin headers and
// modules, size_t can be in either Darwin.C.stddef or _Builtin_stddef.size_t.
// size_t is also defined in macOS by <sys/_types/_size_t.h> which is in the
// Darwin.POSIX._types._size_t module. Ideally Apple will remove the duplicate
// declaration and clang will settle to _Builtin_stddef.size_t, but while it's
// all in flux allow all three of those modules.
// Darwin.C.stddef|_Builtin_stddef.size_t|Darwin.POSIX._types._size_t
// CHECK: ![[DARWIN_MODULE]] = !DIModule({{.*}}, name: "{{stddef|size_t|_size_t}}"
