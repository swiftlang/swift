// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -cxx-interoperability-mode=default -I %S/Inputs/custom-modules %s -o %t/a.out
// RUN: %target-run %t/a.out

import CXXInterop

assert(MemoryLayout<ClassWithImportedField>.size == MemoryLayout<ClassWithUnimportedField>.size,
  "ClassWithImportedField and ClassWithUnimportedField should have the same size.")