// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -I %t \
// RUN:   -module-name Lib -package-name pkg \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -O -wmo -experimental-allow-non-resilient-access -experimental-package-cmo \
// RUN:   -emit-module -emit-module-path %t/Lib.swiftmodule
// RUN: %target-sil-opt %t/Lib.swiftmodule -o %t/Lib.sil

// RUN: %target-swift-frontend -emit-sil -O %t/Client.swift -I %t -package-name pkg \
// RUN: -Xllvm -sil-disable-pass=DeadFunctionAndGlobalElimination -o %t/InPkgClient.sil

// RUN: %target-swift-frontend -emit-sil -O %t/Client.swift -I %t \
// RUN: -Xllvm -sil-disable-pass=DeadFunctionAndGlobalElimination -o %t/ExtClient.sil

/// TEST how functions (and bodies) and tables optimized with [serialized_for_package] in Lib are
/// deserialized into Client depending on package boundary.
///
/// Test 1: They should be deserialized into Client as Lib and Client are in the same package.
// RUN: %FileCheck -check-prefix=CHECK-INPKG %s < %t/InPkgClient.sil

// Pub.__allocating_init(_:)
// CHECK-INPKG: sil public_external @$s3Lib3PubCyACSicfC : $@convention(method) (Int, @thick Pub.Type) -> @owned Pub {
// CHECK-INPKG:     function_ref @$s3Lib3PubCyACSicfc

// Pub.init(_:)
// CHECK-INPKG: sil @$s3Lib3PubCyACSicfc : $@convention(method) (Int, @owned Pub) -> @owned Pub

// CHECK-INPKG: sil_vtable Pub {
// CHECK-INPKG: #Pub.pubVar!getter: (Pub) -> () -> Int : @$s3Lib3PubC6pubVarSivg  // Pub.pubVar.getter
// CHECK-INPKG: #Pub.pubVar!setter: (Pub) -> (Int) -> () : @$s3Lib3PubC6pubVarSivs  // Pub.pubVar.setter
// CHECK-INPKG: #Pub.pubVar!modify: (Pub) -> () -> () : @$s3Lib3PubC6pubVarSivM  // Pub.pubVar.modify
// CHECK-INPKG: #Pub.pkgVar!getter: (Pub) -> () -> Int : @$s3Lib3PubC6pkgVarSivg  // Pub.pkgVar.getter
// CHECK-INPKG: #Pub.pkgVar!setter: (Pub) -> (Int) -> () : @$s3Lib3PubC6pkgVarSivs  // Pub.pkgVar.setter
// CHECK-INPKG: #Pub.pkgVar!modify: (Pub) -> () -> () : @$s3Lib3PubC6pkgVarSivM  // Pub.pkgVar.modify
// CHECK-INPKG: #Pub.init!allocator: (Pub.Type) -> (Int) -> Pub : @$s3Lib3PubCyACSicfC  // Pub.__allocating_init(_:)
// CHECK-INPKG: #Pub.deinit!deallocator: @$s3Lib3PubCfD  // Pub.__deallocating_deinit

// CHECK-INPKG: sil_witness_table public_external Pub: PubProto module Lib {
// CHECK-INPKG:  method #PubProto.pubVar!getter: <Self where Self : PubProto> (Self) -> () -> Int : @$s3Lib3PubCAA0B5ProtoA2aDP6pubVarSivgTW  // protocol witness for PubProto.pubVar.getter in conformance Pub
// CHECK-INPKG:  method #PubProto.pubVar!setter: <Self where Self : PubProto> (inout Self) -> (Int) -> () : @$s3Lib3PubCAA0B5ProtoA2aDP6pubVarSivsTW  // protocol witness for PubProto.pubVar.setter in conformance Pub
// CHECK-INPKG:  method #PubProto.pubVar!modify: <Self where Self : PubProto> (inout Self) -> () -> () : @$s3Lib3PubCAA0B5ProtoA2aDP6pubVarSivMTW  // protocol witness for PubProto.pubVar.modify in conformance Pub


/// Test 2: They should NOT be deserialized into Client as Lib and Client are NOT in the same package;
/// only declaration should be deserialized, not the body.
// RUN: %FileCheck -check-prefix=CHECK-EXT %s < %t/ExtClient.sil

// CHECK-EXT-NOT: sil_vtable Pub {
// CHECK-EXT-NOT: #Pub.pubVar!getter: (Pub) -> () -> Int : @$s3Lib3PubC6pubVarSivg  // Pub.pubVar.getter
// CHECK-EXT-NOT: #Pub.pubVar!setter: (Pub) -> (Int) -> () : @$s3Lib3PubC6pubVarSivs  // Pub.pubVar.setter
// CHECK-EXT-NOT: #Pub.pubVar!modify: (Pub) -> () -> () : @$s3Lib3PubC6pubVarSivM  // Pub.pubVar.modify
// CHECK-EXT-NOT: #Pub.pkgVar!getter: (Pub) -> () -> Int : @$s3Lib3PubC6pkgVarSivg  // Pub.pkgVar.getter
// CHECK-EXT-NOT: #Pub.pkgVar!setter: (Pub) -> (Int) -> () : @$s3Lib3PubC6pkgVarSivs  // Pub.pkgVar.setter
// CHECK-EXT-NOT: #Pub.pkgVar!modify: (Pub) -> () -> () : @$s3Lib3PubC6pkgVarSivM  // Pub.pkgVar.modify
// CHECK-EXT-NOT: #Pub.init!allocator: (Pub.Type) -> (Int) -> Pub : @$s3Lib3PubCyACSicfC  // Pub.__allocating_init(_:)
// CHECK-EXT-NOT: #Pub.deinit!deallocator: @$s3Lib3PubCfD  // Pub.__deallocating_deinit

// CHECK-EXT-NOT: sil_witness_table public_external Pub: PubProto module Lib {
// CHECK-EXT-NOT:  method #PubProto.pubVar!getter: <Self where Self : PubProto> (Self) -> () -> Int : @$s3Lib3PubCAA0B5ProtoA2aDP6pubVarSivgTW  // protocol witness for PubProto.pubVar.getter in conformance Pub
// CHECK-EXT-NOT:  method #PubProto.pubVar!setter: <Self where Self : PubProto> (inout Self) -> (Int) -> () : @$s3Lib3PubCAA0B5ProtoA2aDP6pubVarSivsTW  // protocol witness for PubProto.pubVar.setter in conformance Pub
// CHECK-EXT-NOT:  method #PubProto.pubVar!modify: <Self where Self : PubProto> (inout Self) -> () -> () : @$s3Lib3PubCAA0B5ProtoA2aDP6pubVarSivMTW  // protocol witness for PubProto.pubVar.modify in conformance Pub

// Pub.init(_:) is just a decl; does not contain the body.
// CHECK-EXT: sil @$s3Lib3PubCyACSicfc : $@convention(method) (Int, @owned Pub) -> @owned Pub


/// Verify functions and tables are optimized with Package CMO in Lib.
// RUN: %FileCheck -check-prefix=CHECK-LIB %s < %t/Lib.sil

// CHECK-LIB: sil [serialized] [exact_self_class] [canonical] [ossa] @$s3Lib3PubCyACSicfC : $@convention(method) (Int, @thick Pub.Type) -> @owned Pub {
// CHECK-LIB:  function_ref @$s3Lib3PubCyACSicfc : $@convention(method) (Int, @owned Pub) -> @owned Pub

// Pub.pkgVar.getter
// CHECK-LIB: sil package [serialized_for_package] [canonical] [ossa] @$s3Lib3PubC6pkgVarSivg : $@convention(method) (@guaranteed Pub) -> Int {

// CHECK-LIB: sil_vtable [serialized_for_package] Pub {
// CHECK-LIB:    #Pub.pubVar!getter: (Pub) -> () -> Int : @$s3Lib3PubC6pubVarSivg  // Pub.pubVar.getter
// CHECK-LIB:    #Pub.pubVar!setter: (Pub) -> (Int) -> () : @$s3Lib3PubC6pubVarSivs  // Pub.pubVar.setter
// CHECK-LIB:    #Pub.pubVar!modify: (Pub) -> () -> () : @$s3Lib3PubC6pubVarSivM  // Pub.pubVar.modify
// CHECK-LIB:    #Pub.pkgVar!getter: (Pub) -> () -> Int : @$s3Lib3PubC6pkgVarSivg  // Pub.pkgVar.getter
// CHECK-LIB:    #Pub.pkgVar!setter: (Pub) -> (Int) -> () : @$s3Lib3PubC6pkgVarSivs  // Pub.pkgVar.setter
// CHECK-LIB:    #Pub.pkgVar!modify: (Pub) -> () -> () : @$s3Lib3PubC6pkgVarSivM  // Pub.pkgVar.modify
// CHECK-LIB:    #Pub.init!allocator: (Pub.Type) -> (Int) -> Pub : @$s3Lib3PubCyACSicfC  // Pub.__allocating_init(_:)
// CHECK-LIB:    #Pub.deinit!deallocator: @$s3Lib3PubCfD  // Pub.__deallocating_deinit

// CHECK-LIB: sil_witness_table [serialized_for_package] Pub: PubProto module Lib {
// CHECK-LIB:    method #PubProto.pubVar!getter: <Self where Self : PubProto> (Self) -> () -> Int : @$s3Lib3PubCAA0B5ProtoA2aDP6pubVarSivgTW  // protocol witness for PubProto.pubVar.getter in conformance Pub
// CHECK-LIB:    method #PubProto.pubVar!setter: <Self where Self : PubProto> (inout Self) -> (Int) -> () : @$s3Lib3PubCAA0B5ProtoA2aDP6pubVarSivsTW  // protocol witness for PubProto.pubVar.setter in conformance Pub
// CHECK-LIB:    method #PubProto.pubVar!modify: <Self where Self : PubProto> (inout Self) -> () -> () : @$s3Lib3PubCAA0B5ProtoA2aDP6pubVarSivMTW  // protocol witness for PubProto.pubVar.modify in conformance Pub


//--- Lib.swift

public protocol PubProto {
  var pubVar: Int { get set }
}

public class Pub: PubProto {
  public var pubVar: Int = 1
  package var pkgVar: Int = 2
  var internalVar: Int = 3
  public init(_ arg: Int) {
    pubVar = arg
    pkgVar = arg
  }
  public func pubFunc(_ arg: Pub) {
    print(arg.pubVar)
  }
}

public class SubPub: Pub {
  override public func pubFunc(_ arg: Pub) {
    print(arg.pubVar, arg.internalVar)
  }
}

//--- Client.swift
import Lib

public func test1(_ arg: Int) -> Int {
  let x = Pub(arg)
  x.pubVar = 3
  return x.run()
}

public func test2(_ arg: Int) {
  SubPub(arg).pubFunc(Pub(arg))
}

public extension PubProto {
  func run() -> Int {
    return pubVar
  }
}

