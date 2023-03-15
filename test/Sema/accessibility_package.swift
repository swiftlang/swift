// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -module-name Utils %t/Utils.swift -emit-module -emit-module-path %t/Utils.swiftmodule -package-name myLib
// RUN: test -f %t/Utils.swiftmodule

// RUN: %target-swift-frontend -module-name Lib %t/Lib.swift -emit-module -emit-module-path %t/Lib.swiftmodule -package-name myLib -I %t
// RUN: test -f %t/Lib.swiftmodule

// RUN: not %target-swift-frontend -typecheck %t/Lib.swift -package-name "otherLib" -I %t 2>&1 | %FileCheck %s
// CHECK: error: cannot find type 'PackageProto' in scope
// CHECK: error: 'pkgFunc' is inaccessible due to 'package' protection level
// CHECK: error: cannot find 'PackageKlass' in scope


// BEGIN Utils.swift
package protocol PackageProto {
  var pkgVar: String { get set }
}

package class PackageKlass {
  package init() {}
  package func pkgFunc() {}
}

public class PublicKlass {
  public init() {}
  public func publicFunc() {}
  package func pkgFunc() {}
}


// BEGIN Lib.swift
import Utils

public func start() {
  let x = PublicKlass()
  x.publicFunc()
  x.pkgFunc()

  let y = PackageKlass()
  y.pkgFunc()
}

package struct LibStruct : PackageProto {
  package var pkgVar: String = "lib"
}

