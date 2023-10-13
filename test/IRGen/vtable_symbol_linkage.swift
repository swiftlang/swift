// RUN: %empty-directory(%t)

// RUN: %target-build-swift %S/Inputs/vtable_symbol_linkage_base.swift -emit-module -emit-module-path=%t/BaseModule.swiftmodule -emit-library -module-name BaseModule -o %t/%target-library-name(BaseModule)
// RUN: %target-build-swift -I %t %s -o %t/a.out -L%t -lBaseModule

// RUN: %target-build-swift %S/Inputs/vtable_symbol_linkage_base.swift -emit-module -emit-module-path=%t/BaseModule.swiftmodule -emit-library -module-name BaseModule -o %t/%target-library-name(BaseModule) -enable-library-evolution
// RUN: %target-build-swift -I %t %s -o %t/a.out -L%t -lBaseModule

// UNSUPPORTED: CPU=wasm32

// Check if the program can be linked without undefined symbol errors.

import BaseModule

public class Derived : Base {}

public class MostDerived : Middle {}

public class DerivedNested : Namespace.Nested {}
public class DerivedExtNested : Namespace.ExtNested {}
