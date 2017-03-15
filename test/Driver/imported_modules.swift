// RUN: %target-build-swift -emit-imported-modules -module-name test %s %S/Inputs/imported_modules/imported_modules2.swift -o %t.importedmodules
// RUN: diff %t.importedmodules %S/Inputs/imported_modules/imported_modules.importedmodules

import A.B.C
import X
import Y
import enum Foo.Member
