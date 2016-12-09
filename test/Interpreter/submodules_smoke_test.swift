// RUN: %target-build-swift -typecheck %s -F %S/Inputs -Xfrontend -verify
// RUN: %target-build-swift -emit-ir -g %s -F %S/Inputs -DNO_ERROR > /dev/null
// REQUIRES: executable_test

import ExplicitSubmodulesOnly.B
_ = constantB

#if !NO_ERROR
_ = constantA // expected-error{{use of unresolved identifier 'constantA'}}
#endif
