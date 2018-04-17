// RUN: %target-build-swift -emit-imported-modules -module-name test %s %S/Inputs/imported_modules/imported_modules2.swift -o %t.importedmodules
// RUN: diff %t.importedmodules %S/Inputs/imported_modules/imported_modules.importedmodules

import A.B.C
import X
import Y
import enum Foo.Member
// The overlaying Swift module should not be loaded.
import InvalidOverlay

#if canImport(Swift) // To wit, true
import Swift
#else
import Garbage
#endif

#if !canImport(Swift) // To wit, false
import Garbage
#endif
