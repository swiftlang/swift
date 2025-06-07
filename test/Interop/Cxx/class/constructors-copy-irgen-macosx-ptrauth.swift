// RUN: %swift -emit-ir -module-name CopyConstructorsPtrAuth -target arm64e-apple-macosx11.0 -use-clang-function-types -Xcc -D -Xcc ENABLE_PTRAUTH -dump-clang-diagnostics -I %S/Inputs -cxx-interoperability-mode=default %s -parse-as-library

// REQUIRES: CPU=arm64e
// REQUIRES: OS=macosx

import Constructors

public func copyPtrAuthMemberWithDefaultCopyConstructor(_ x: HasPtrAuthMember)
  -> (HasPtrAuthMember, HasPtrAuthMember) {
  return (x, x)
}
