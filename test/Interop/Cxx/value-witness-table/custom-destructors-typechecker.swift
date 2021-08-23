// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-cxx-interop

import CustomDestructor

_ = HasUserProvidedDestructor()
_ = HasEmptyDestructorAndMemberWithUserDefinedConstructor()
_ = HasNonTrivialImplicitDestructor()
_ = HasNonTrivialDefaultedDestructor()
