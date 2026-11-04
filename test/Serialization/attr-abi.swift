// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Library.swift -module-name Library \
// RUN:   -parse-as-library -enable-library-evolution -o %t/Library.swiftmodule
// RUN: %target-swift-frontend -emit-silgen %t/Client.swift -I %t \
// RUN:   -module-name Client -parse-as-library | %FileCheck %s --check-prefix=SILGEN
// RUN: %target-swift-ide-test -print-module -module-to-print=Library -I %t \
// RUN:   -source-filename=%s | %FileCheck %s --check-prefix=MODULE

// A module compiled in script mode wraps top-level variables in top-level code.
// RUN: %target-swift-frontend -emit-module %t/Script.swift -module-name Script \
// RUN:   -swift-version 6 -o %t/Script.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/ScriptClient.swift -I %t

// MODULE:      @abi(var abiStored: Int){{$}}
// MODULE-NEXT: var stored: Int{{$}}
// MODULE:      @abi(var abiComputed: Int){{$}}
// MODULE-NEXT: var computed: Int{{$}}
// MODULE:      @abi(subscript(abi i: Int) -> Int){{$}}
// MODULE-NEXT: subscript(i: Int) -> Int { get }{{$}}
// MODULE:      @abi(static var abiStaticStored: Int){{$}}
// MODULE-NEXT: static var staticStored: Int{{$}}
// MODULE:      @abi(var abiComputedGlobal: Int){{$}}
// MODULE-NEXT: var computedGlobal: Int { get }{{$}}
// MODULE:      @abi(func abiFunction()){{$}}
// MODULE-NEXT: func function(){{$}}
// MODULE:      @abi(var abiStoredGlobal: Int){{$}}
// MODULE-NEXT: var storedGlobal: Int{{$}}

//--- Library.swift

@abi(var abiStoredGlobal: Int)
public var storedGlobal: Int = 0

@abi(var abiComputedGlobal: Int)
public var computedGlobal: Int { 0 }

@abi(func abiFunction())
public func function() {}

public struct S {
  public init() {}

  @abi(var abiStored: Int)
  public var stored: Int = 0

  @abi(var abiComputed: Int)
  public var computed: Int {
    get { 0 }
    set {}
  }

  @abi(subscript(abi i: Int) -> Int)
  public subscript(i: Int) -> Int { 0 }

  @abi(static var abiStaticStored: Int)
  public static var staticStored: Int = 0
}

//--- Client.swift

import Library

// SILGEN-LABEL: sil hidden [ossa] @$s6Client4testyy7Library1SVzF
func test(_ s: inout S) {
  // SILGEN: function_ref @$s7Library11abiFunctionyyF
  function()
  // SILGEN: function_ref @$s7Library17abiComputedGlobalSivg
  // SILGEN: function_ref @$s7Library15abiStoredGlobalSivs
  storedGlobal = computedGlobal
  // SILGEN: function_ref @$s7Library1SV11abiComputedSivg
  // SILGEN: function_ref @$s7Library1SV9abiStoredSivs
  s.stored = s.computed
  // SILGEN: function_ref @$s7Library1SV3abiS2i_tcig
  // SILGEN: function_ref @$s7Library1SV11abiComputedSivs
  s.computed = s[0]
  // SILGEN: function_ref @$s7Library1SV15abiStaticStoredSivsZ
  S.staticStored = 0
}

//--- Script.swift

@abi(var abiStoredGlobal: Int)
public var storedGlobal: Int = 0

@abi(var abiComputedGlobal: Int)
public var computedGlobal: Int { 0 }

//--- ScriptClient.swift

import Script

@MainActor
func test() {
  storedGlobal = computedGlobal
}
