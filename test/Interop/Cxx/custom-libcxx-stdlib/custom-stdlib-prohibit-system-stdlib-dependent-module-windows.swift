// RUN: %empty-directory(%t)
// RUN: %target-swiftxx-frontend -DONE -emit-module -I %S/Inputs %s -module-name One -o %t/One.swiftmodule
// RUN: %target-swiftxx-frontend -typecheck -I %t -Xcc -cxx-isystem -Xcc %S/Inputs/c++ -Xcc -stdlib=libc++ -Xcc -nostdinc++ -I %S/Inputs -dump-clang-diagnostics %s -verify

// REQUIRES: OS=windows-msvc

#if ONE
import CxxHeaderSystemStdlib
#else
import One // expected-error {{module 'One' was built with msvcprt, but current compilation uses libc++}}
#endif
