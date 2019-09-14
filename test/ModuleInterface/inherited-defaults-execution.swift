// REQUIRES: executable_test
// RUN: %empty-directory(%t)

// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic
// UNSUPPORTED: swift_test_mode_optimize_with_implicit_dynamic

// 1) Build the 'Inherited' library and its interface from this file
//
// RUN: %target-build-swift-dylib(%t/%target-library-name(Inherited)) -emit-module-path %t/Inherited.swiftmodule -emit-module-interface-path %t/Inherited.swiftinterface -module-name Inherited %s
// RUN: rm %t/Inherited.swiftmodule

// 2) Check the interface includes the synthesized initializers of the base
//    class in the derived class explicitly and uses the '= super' syntax to
//    inherit its default arguments.
//
// RUN: cat %t/Inherited.swiftinterface | %FileCheck --check-prefix=INTERFACE %s
//
// INTERFACE: public class Base {
// INTERFACE:   public init(x: Swift.Int = 45, y: Swift.Int = 98)
// INTERFACE: }
// INTERFACE: public class Derived : Inherited.Base {
// INTERFACE:   override public init(x: Swift.Int = super, y: Swift.Int = super)
// INTERFACE: }

// 4) Generate a main.swift file that uses the 'Inherited' library and makes use
//    of the inherited default arguments
//
// RUN: echo "import Inherited" > %t/main.swift
// RUN: echo "print(Derived().x)" >> %t/main.swift
// RUN: echo "print(Derived().y)" >> %t/main.swift

// 5) Build and run the executable, checking the defaulted arguments resulted in
//    the correct values being stored
//
// RUN: %target-build-swift -I%t -L%t -lInherited -o %t/main %target-rpath(%t) %t/main.swift -swift-version 5
// RUN: %target-codesign %t/main %t/%target-library-name(Inherited)
// RUN: %target-run %t/main %t/%target-library-name(Inherited) | %FileCheck --check-prefix=OUTPUT %s
//
// OUTPUT: 45
// OUTPUT-NEXT: 98

public class Base {
  public let x: Int
  public let y: Int
  public init(x: Int = 45, y: Int = 98) {
    self.x = x
    self.y = y
  }
}
public class Derived: Base {}
