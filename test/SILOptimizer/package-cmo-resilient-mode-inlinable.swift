// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift %t/Lib.swift \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -parse-as-library -emit-module -emit-module-path %t/Lib.swiftmodule -I%t \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -O -wmo -enable-library-evolution
// RUN: %target-sil-opt %t/Lib.swiftmodule -sil-verify-all -o %t/Lib.sil

// RUN: %target-build-swift -module-name=Main -package-name Pkg -I%t -emit-sil -O %t/main.swift -o %t/Main.sil

// RUN: %FileCheck %s --check-prefix=CHECK < %t/Lib.sil
// RUN: %FileCheck %s --check-prefix=CHECK-MAIN < %t/Main.sil

// REQUIRES: swift_in_compiler

//--- main.swift

import Lib

// CHECK-MAIN: sil @$s4Main10inUseInPuby3Lib0E6StructVSiF : $@convention(thin) (Int) -> @out PubStruct {
@inlinable
public func inUseInPub(_ arg: Int) -> PubStruct {
  // CHECK-MAIN: struct $PubStruct
  // CHECK-MAIN: store {{.*}} to %0 : $*PubStruct
  inLib(arg)
}

// CHECK-MAIN: sil @$s4Main8inUsePuby3Lib0D6StructVSiF : $@convention(thin) (Int) -> @out PubStruct {
@inlinable
public func inUsePub(_ arg: Int) -> PubStruct {
  // CHECK-MAIN: struct $PubStruct
  // CHECK-MAIN: store {{.*}} to %0 : $*PubStruct
  libPub(arg)
}

// CHECK-MAIN: sil @$s4Main8useInPuby3Lib0D6StructVSiF : $@convention(thin) (Int) -> @out PubStruct {
public func useInPub(_ arg: Int) -> PubStruct {
  // CHECK-MAIN: struct $PubStruct
  // CHECK-MAIN: store {{.*}} to %0 : $*PubStruct
  inLib(arg)
}

// CHECK-MAIN: sil @$s4Main6usePuby3Lib0C6StructVSiF : $@convention(thin) (Int) -> @out PubStruct {
public func usePub(_ arg: Int) -> PubStruct {
  // CHECK-MAIN: struct $PubStruct
  // CHECK-MAIN: store {{.*}} to %0 : $*PubStruct
  libPub(arg)
}

// CHECK-MAIN: sil @$s4Main8useInPkgy3Lib03UfiD6StructVSiF : $@convention(thin) (Int) -> @out UfiPkgStruct {
@inlinable
package func useInPkg(_ arg: Int) -> UfiPkgStruct {
  // CHECK-MAIN: struct $UfiPkgStruct
  // CHECK-MAIN: store {{.*}} to %0 : $*UfiPkgStruct
  libInPkg(arg)
}

// CHECK-MAIN: sil package @$s4Main6usePkgy3Lib0C6StructVSiF : $@convention(thin) (Int) -> @out PkgStruct {
package func usePkg(_ arg: Int) -> PkgStruct {
  // CHECK-MAIN: struct $PkgStruct
  // CHECK-MAIN: store {{.*}} to %0 : $*PkgStruct
  libPkg(arg)
}


//--- Lib.swift

// CHECK: sil [serialized_for_package] [canonical] @$s3Lib02inA0yAA9PubStructVSiF : $@convention(thin) (Int) -> @out PubStruct {
@inlinable
public func inLib(_ arg: Int) -> PubStruct {
  // CHECK: function_ref @$s3Lib9PubStructVyACSicfC : $@convention(method) (Int, @thin PubStruct.Type) -> @out PubStruct
  // CHECK: function_ref @$s3Lib9PubStructV3pubSivM : $@yield_once @convention(method) (@inout PubStruct) -> @yields @inout Int
  var p = PubStruct(1)
  p.pub += arg
  return p
}

// CHECK: sil [serialized_for_package] [canonical] @$s3Lib9PubStructVyACSicfC : $@convention(method) (Int, @thin PubStruct.Type) -> @out PubStruct {
// CHECK: struct $PubStruct
// CHECK: store {{.*}} to %0 : $*PubStruct

// CHECK: sil [serialized_for_package] [canonical] @$s3Lib12UfiPkgStructVyACSicfC : $@convention(method) (Int, @thin UfiPkgStruct.Type) -> @out UfiPkgStruct {
// CHECK: struct $UfiPkgStruct
// CHECK: store {{.*}} to %0 : $*UfiPkgStruct

// CHECK: sil package [serialized_for_package] [canonical] @$s3Lib6libPkgyAA0C6StructVSiF : $@convention(thin) (Int) -> @out PkgStruct {
package func libPkg(_ arg: Int) -> PkgStruct {
  // CHECK: struct $PkgStruct
  // CHECK: store {{.*}} to %0 : $*PkgStruct
  var p = PkgStruct(1)
  p.pkg += arg
  return p
}

// CHECK: sil [serialized_for_package] [canonical] @$s3Lib6libPubyAA0C6StructVSiF : $@convention(thin) (Int) -> @out PubStruct {
public func libPub(_ arg: Int) -> PubStruct {
  // CHECK: struct $PubStruct
  // CHECK: store {{.*}} to %0 : $*PubStruct
  var p = PubStruct(1)
  p.pub += arg
  return p
}

// CHECK: sil [serialized_for_package] [canonical] @$s3Lib8libInPkgyAA03UfiD6StructVSiF : $@convention(thin) (Int) -> @out UfiPkgStruct {
@inlinable
package func libInPkg(_ arg: Int) -> UfiPkgStruct {
  // CHECK: function_ref @$s3Lib12UfiPkgStructVyACSicfC : $@convention(method) (Int, @thin UfiPkgStruct.Type) -> @out UfiPkgStruct
  var p = UfiPkgStruct(1)
  p.ufiPkg += arg
  return p
}

@inlinable
func libInHid(_ arg: Int) -> UfiHidStruct {
  var p = UfiHidStruct(1)
  p.ufiHid += arg
  return p
}

public struct PubStruct {
  public var pub: Int
  public init(_ arg: Int) {
    pub = arg
  }
  public func pubFunc(_ arg: Int) -> Int {
    return arg > 0 ? arg : arg + 11
  }
}

package struct PkgStruct {
  package var pkg: Int
  package init(_ arg: Int) {
    pkg = arg
  }
  package func pkgFunc(_ arg: Int) -> Int {
    return arg > 0 ? arg : 11
  }
}

@usableFromInline
package struct UfiPkgStruct {
  @usableFromInline
  package var ufiPkg: Int
  @usableFromInline
  package init(_ arg: Int) {
    ufiPkg = arg
  }
  @usableFromInline
  package func ufiPkgFunc(_ arg: Int) -> Int {
    return arg > 0 ? arg : 11
  }
}

@usableFromInline
struct UfiHidStruct {
  @usableFromInline
  var ufiHid: Int
  @usableFromInline
  init(_ arg: Int) {
    ufiHid = arg
  }
  @usableFromInline
  func ufiFunc(_ arg: Int) -> Int {
    return arg > 0 ? arg : 11
  }
}

struct HiddenStruct {
  var hidden: Int
  init(_ arg: Int) {
    hidden = arg
  }
  func hiddenFunc(_ arg: Int) -> Int {
    return arg > 0 ? arg : 11
  }
}
