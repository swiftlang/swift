// RUN: %empty-directory(%t)

// RUN: %target-build-swift %S/Inputs/vtable_symbol_linkage_base.swift -emit-module -emit-module-path=%t/BaseModule.swiftmodule -emit-library -module-name BaseModule -o %t/BaseModule.%target-dylib-extension
// RUN: %target-build-swift -I %t %s %t/BaseModule.%target-dylib-extension -o %t/a.out

// RUN: %target-build-swift %S/Inputs/vtable_symbol_linkage_base.swift -emit-module -emit-module-path=%t/BaseModule.swiftmodule -emit-library -module-name BaseModule -o %t/BaseModule.%target-dylib-extension -Xfrontend -enable-resilience -Xfrontend -enable-class-resilience
// RUN: %target-build-swift -I %t %s %t/BaseModule.%target-dylib-extension -o %t/a.out -Xfrontend -enable-class-resilience

// Check if the program can be linked without undefined symbol errors.

import BaseModule

public class Derived : Base {
}

public class DerivedNested : Namespace.Nested {}
public class DerivedExtNested : Namespace.ExtNested {}
