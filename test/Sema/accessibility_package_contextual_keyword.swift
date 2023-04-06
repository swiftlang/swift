// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend-typecheck -verify -disable-availability-checking %t/main.swift -package-name myPkg
// RUN: %target-swift-frontend-typecheck -verify -disable-availability-checking %t/A.swift -package-name myPkg
// RUN: %target-swift-frontend-typecheck -verify -disable-availability-checking %t/B.swift -package-name myPkg
// RUN: %target-swift-frontend-typecheck -verify -disable-availability-checking %t/D.swift -package-name myPkg
// RUN: not %target-swift-frontend-typecheck -verify -disable-availability-checking %t/C.swift -package-name myPkg 2>&1 | %FileCheck %s

//--- main.swift
package(set) public var a: String // should pass when `package` modifier is used at top level decls
public package(set) var b: String
package let c: Int
package var d: Int
package func f() {}
package func package() {}
package()

//--- A.swift
package class package { // package can be a type name
  package init() {}
  package var package: String?  // package can be a var name
}

package class pkg {
  package init() {}
  package func package() {} // package can be a func name
  package func package(arg: Int) {}
  package func package(_ arg: Double) {}
}

public class MyClass {
  var myVar1: package = package()
  var myVar2: pkg = pkg()
  func myFunc() {
    _ = myVar1.package
    myVar2.package()
    myVar2.package(arg: 1)
    myVar2.package(2.0)
  }
}

//--- B.swift
public class Foo {
  package(set) public var x: String?
  public package(set) var y: Int?
}

//--- C.swift
public class Bar {
  package package(set) package: String? // CHECK: warning: 'package(set)' modifier is redundant for a package var
  package(set) package package: String? // CHECK: warning: 'package(set)' modifier is redundant for a package var
}

//--- D.swift
enum MyColor {
  case red, green, blue
}
let packages: [MyColor] = [MyColor.red, MyColor.blue].compactMap { package in
  switch package {
    case .blue: return package // Should not error when `case` follows the `package` contextual keyword
    case .red: return package
    default: return nil
  }
}

