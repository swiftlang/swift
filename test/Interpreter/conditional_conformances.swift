// RUN: %empty-directory(%t)
// The file that's `main` needs to be called that.
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -D basic %t/main.swift %S/../Inputs/conditional_conformance_basic_conformances.swift -o %t/basic && %target-codesign %t/basic && %target-run %t/basic
// RUN: %target-build-swift -D with_assoc %t/main.swift %S/../Inputs/conditional_conformance_with_assoc.swift -o %t/with_assoc && %target-codesign %t/with_assoc && %target-run %t/with_assoc
// RUN: %target-build-swift -D subclass %t/main.swift %S/../Inputs/conditional_conformance_subclass.swift -o %t/subclass && %target-codesign %t/subclass && %target-run %t/subclass

// REQUIRES: executable_test

// Call the various functions from each of the IRGen conditional conformance
// tests. These can't be put in the file themselves because that changes it from
// being treated as a library to an executable.

#if basic
single_generic(IsP2.self)
single_concrete()

double_generic_generic(IsP2.self, IsP3.self)
double_generic_concrete(IsP2.self)
double_concrete_concrete()

assert(dynamicCastToP1(Single<IsP3>()) == nil)
assert(dynamicCastToP1(Single<IsP2>()) != nil)

#elseif with_assoc
generic_generic(IsAlsoP2.self, IsP3.self)
generic_concrete(IsAlsoP2.self)
concrete_generic(IsP3.self)
concrete_concrete()

#elseif subclass
subclassgeneric_generic(IsP2.self)
subclassgeneric_concrete()
subclassconcrete()
subclassgenericconcrete()
#endif


