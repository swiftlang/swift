// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift %t/Lib.swift \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -parse-as-library -emit-module -emit-module-path %t/Lib.swiftmodule -I%t \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -O -wmo -enable-library-evolution
// RUN: %target-sil-opt %t/Lib.swiftmodule -sil-verify-all -o %t/Lib.sil

// RUN: %target-build-swift -module-name=Main -package-name Pkg -I%t -emit-sil -O %t/main.swift -o %t/Main.sil

// RUN: %FileCheck %s --check-prefixes=CHECK < %t/Lib.sil
// RUN: %FileCheck %s --check-prefixes=CHECK-MAIN < %t/Main.sil


//--- main.swift

import Lib

/// There should be no linker error on a public function 
/// that contains symbols internal to Lib module.
// CHECK-MAIN: function_ref @$s3Lib11useInternalySiAA4BaseCF : $@convention(thin) (@guaranteed Base) -> Int
let x = useInternal(Base())

/// Since Base is not serialized, accessing its field should go 
/// through class_method.
// CHECK-MAIN: class_method %13 : $Base, #Base.baseVarPkg!getter : (Base) -> () -> Int, $@convention(method) (@guaranteed Base) -> Int
let y = usePkg(Base())

/// Since PubKlass is serialized, can access its field directly.
// CHECK-MAIN: struct $Int
// CHECK-MAIN-NEXT: store
let z = usePub(PubKlass())

// useInternal(_:)
// CHECK-MAIN: sil @$s3Lib11useInternalySiAA4BaseCF : $@convention(thin) (@guaranteed Base) -> Int



//--- Lib.swift

/// Package CMO does not serialize this function since
/// it references an internal symbol `baseVarInternal`.
/// If it were [serialized_for_pkg], it would leak into
/// client and client won't be able to find the symbol
/// `baseVarInternal` since its dispatch thunk was not
/// generated in the first place (due to it being internal). 
// CHECK-NOT: s3Lib11useInternalySiAA4BaseCF
public func useInternal(_ arg: Base) -> Int {
  return PubKlass().pkgVar + arg.baseVarInternal
}

// CHECK-DAG: sil package [serialized_for_package] [canonical] @$s3Lib6usePkgySiAA4BaseCF : $@convention(thin) (@guaranteed Base) -> Int {
package func usePkg(_ arg: Base) -> Int {
  return arg.baseVarPkg
}

// CHECK-DAG: sil [serialized_for_package] [canonical] @$s3Lib6usePubySiAA0C5KlassCF : $@convention(thin) (@guaranteed PubKlass) -> Int {
public func usePub(_ arg: PubKlass) -> Int {
  return arg.pubVar
} 

/// This class is not serialized since it contains
/// an internal field.
public class Base {
  public init() {}
  var baseVarInternal: Int {
    return 0
  }
  package var baseVarPkg: Int {
    return 0
  }
}

@usableFromInline 
class UFIKlass: Base {
  override init() {}

  var varInternal = 11

  @usableFromInline
  var varUfi = 12

  override var baseVarInternal: Int { return 1 }
  override var baseVarPkg: Int { return 2 }
}

public class PubKlass {
  public init() {}
  public var pubVar: Int {
    return 1
  }
  package var pkgVar: Int {
    return 2
  }
}

/// PubKlass doesn't contain internal symbols so its vtable is serialized.
// CHECK-LABEL: sil_vtable [serialized_for_package] PubKlass {
// CHECK-NEXT:  #PubKlass.init!allocator: (PubKlass.Type) -> () -> PubKlass : @$s3Lib8PubKlassCACycfC
// CHECK-NEXT: #PubKlass.pubVar!getter: (PubKlass) -> () -> Int : @$s3Lib8PubKlassC6pubVarSivg
// CHECK-NEXT: #PubKlass.pkgVar!getter: (PubKlass) -> () -> Int : @$s3Lib8PubKlassC6pkgVarSivg
// CHECK-NEXT: #PubKlass.deinit!deallocator: @$s3Lib8PubKlassCfD
