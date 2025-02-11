// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// 1. Test `@_implementationOnly import`.
// RUN: %target-build-swift-dylib(%t/%target-library-name(Utils)) \
// RUN: %t/UtilsA.swift %t/UtilsB.swift \
// RUN: -module-name Utils -emit-module -package-name Pkg \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -enable-library-evolution -O -wmo
// RUN: %target-sil-opt %t/Utils.swiftmodule -I %t -sil-verify-all -o %t/Utils.sil
// RUN: %FileCheck %s --check-prefix=CHECK-UTILS < %t/Utils.sil

/// Verify accessing PkgKlass.second from a client in a dynamic context does not cause a linker error.
// RUN: %target-build-swift -I %t -L %t %t/Client.swift -package-name Pkg \
// RUN: -O -wmo -enable-library-evolution \
// RUN: %target-rpath(%t) -lUtils -o %t/a.out

// RUN: %target-swift-frontend -emit-sil -I %t -L %t %t/Client.swift \
// RUN: -package-name Pkg -O -wmo -enable-library-evolution \
// RUN: -lUtils -o %t/Client.sil
// RUN: %FileCheck %s --check-prefix=CHECK-CLIENT < %t/Client.sil

/// 2. Test `package import` and `@_spiOnly import`.
// RUN: %target-swift-frontend %t/CoreA.swift \
// RUN: -module-name=CoreA -package-name Pkg \
// RUN: -parse-as-library -emit-module \
// RUN: -emit-module-path %t/CoreA.swiftmodule -I%t \
// RUN: -O -wmo -enable-library-evolution

// RUN: %target-swift-frontend %t/CoreB.swift \
// RUN: -module-name=CoreB -package-name Pkg \
// RUN: -parse-as-library -emit-module \
// RUN: -emit-module-path %t/CoreB.swiftmodule -I%t \
// RUN: -O -wmo -enable-library-evolution

// RUN: %target-swift-frontend %t/UI.swift \
// RUN: -module-name=UI -package-name Pkg \
// RUN: -parse-as-library -emit-module \
// RUN: -experimental-spi-only-imports \
// RUN: -emit-module-path %t/UI.swiftmodule -I %t \
// RUN: -experimental-package-cmo -experimental-allow-non-resilient-access \
// RUN: -O -wmo -enable-library-evolution -Rmodule-loading 2> %t/UI-result.txt
// RUN: %target-sil-opt %t/UI.swiftmodule -I %t -sil-verify-all -o %t/UI.sil
// RUN: %FileCheck %s < %t/UI.sil

// REQUIRES: swift_in_compiler
// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos || OS=maccatalyst

//--- Client.swift
package import Utils

package func clientFunc<T: PkgKlass>(_ list: [T]) {
  // closure #1 in clientFunc<A>(_:)
  // CHECK-CLIENT: sil private @$s6Client10clientFuncyySayxG5Utils8PkgKlassCRbzlFSo8NSObjectCxXEfU_ : $@convention(thin) <T where T : PkgKlass> (@in_guaranteed T) -> (@out NSObject, @error_indirect Never) {
  // CHECK-CLIENT: class_method {{.*}} #PkgKlass.second!getter : (PkgKlass) -> () -> NSObject, $@convention(method) (@guaranteed PkgKlass) -> @owned NSObject
  // CHECK-CLIENT: } // end sil function '$s6Client10clientFuncyySayxG5Utils8PkgKlassCRbzlFSo8NSObjectCxXEfU_'
  let result = list.map { $0.second }
  print(result)
}


//--- UtilsA.swift
public import Foundation // public import to allow `NSObject` in API.

package class PkgKlass: NSObject {
  /// Serialized since it does _not_ reference a type from module imported as @_implementationOnly.
  // PkgKlass.first.getter
  // CHECK-UTILS-DAG: sil package [serialized_for_package] [canonical] [ossa]  @$s5Utils8PkgKlassC5firstSSvg : $@convention(method) (@guaranteed PkgKlass) -> @owned String {
  package var first: String

  /// NOT serialized since it does reference a type from module imported as @_implementationOnly.
  // PkgKlass.second.getter
  // CHECK-UTILS-DAG: sil package_external [canonical] @$s5Utils8PkgKlassC6secondSo8NSObjectCvg : $@convention(method) (@guaranteed PkgKlass) -> @owned NSObject
  @objc package var second: NSObject

  init(first: String, second: NSObject) {
      self.first = first
      self.second = second
  }
}

//--- UtilsB.swift
@_implementationOnly import Foundation

public func utilsFunc() {
  let x: NSString = "utilsfunc"
  print(x)
}

//--- UI.swift
package import CoreA
@_spiOnly public import CoreB

/// PkgStruct is imported with `package import` and should be serialized.
// CHECK-DAG: sil package [serialized_for_package] [canonical] [ossa] @$s2UI6uiFuncyy5CoreA9PkgStructVF : $@convention(thin) (@in_guaranteed PkgStruct) -> () {
package func uiFunc(_ arg: PkgStruct) {
  print(arg.pkgVar)
}

/// PubStruct is imported with `@_spiOnly public import` and should be serialized.
// CHECK-DAG: sil [serialized_for_package] [canonical] [ossa] @$s2UI7spiFuncyy5CoreB15PubStructForSPIVF : $@convention(thin) (@in_guaranteed PubStructForSPI) -> () {
@_spi(GroupB)
public func spiFunc(_ arg: PubStructForSPI) {
  print(arg.pubVarForSPI)
}

//--- CoreA.swift
package struct PkgStruct {
  package var pkgVar: Int
  package init(_ arg: Int) {
    self.pkgVar = arg
  }
}

//--- CoreB.swift
public struct PubStructForSPI {
  public var pubVarForSPI: String
  public init(_ arg: String) {
    self.pubVarForSPI = arg
  }
}
