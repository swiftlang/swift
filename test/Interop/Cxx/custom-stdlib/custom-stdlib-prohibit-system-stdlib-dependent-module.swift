// RUN: rm -rf %t
// RUN: %target-swiftxx-frontend -DONE -emit-module -I %S/Inputs %s -module-name One -o %t/One.swiftmodule

// RUN: %target-swiftxx-frontend -typecheck -I %t -cxx-stdlib-path%S/Inputs/c++ -I %S/Inputs -dump-clang-diagnostics %s -verify

#if ONE
import CxxHeaderSystemStdlib
#else
import One // expected-error {{module 'One' uses system's C++ standard library, but current compilation uses a custom C++ standard library}}
#endif
