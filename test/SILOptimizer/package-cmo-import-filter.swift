// RUN: %empty-directory(%t)
// RUN: split-file %s %t

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

// RUN: %target-swift-frontend %t/Lib.swift \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -parse-as-library -emit-module \
// RUN: -experimental-spi-only-imports \
// RUN: -emit-module-path %t/Lib.swiftmodule -I %t \
// RUN: -experimental-package-cmo -experimental-allow-non-resilient-access \
// RUN: -O -wmo -enable-library-evolution -Rmodule-loading 2> %t/Lib-result.txt
// RUN: %target-sil-opt %t/Lib.swiftmodule -I %t -sil-verify-all -o %t/Lib.sil
// RUN: %FileCheck %s < %t/Lib.sil

// REQUIRES: swift_in_compiler


//--- Lib.swift
package import CoreA
@_spiOnly public import CoreB

/// PkgStruct is imported with `package import` and should be serialized.
// CHECK-DAG: sil package [serialized_for_package] [canonical] [ossa] @$s3Lib7libFuncyy5CoreA9PkgStructVF : $@convention(thin) (@in_guaranteed PkgStruct) -> () {
package func libFunc(_ arg: PkgStruct) {
  print(arg.pkgVar)
}

/// PubStruct is imported with `@_spiOnly public import` and should be serialized.
// CHECK-DAG: sil [serialized_for_package] [canonical] [ossa] @$s3Lib7spiFuncyy5CoreB15PubStructForSPIVF : $@convention(thin) (@in_guaranteed PubStructForSPI) -> () {
@_spi(InCoreB)
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
