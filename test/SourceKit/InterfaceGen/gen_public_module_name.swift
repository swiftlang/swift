// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// RUN: %target-swift-frontend %t/common.swift -sdk %t/sdk \
// RUN:   -module-name Common \
// RUN:   -emit-module-path %t/sdk/Common.swiftmodule \
// RUN:   -emit-module-interface-path %t/sdk/Common.swiftinterface \
// RUN:   -enable-library-evolution -swift-version 6

// RUN: %target-swift-frontend %t/subimpl.swift -sdk %t/sdk -I %t/sdk \
// RUN:   -module-name SubImpl \
// RUN:   -emit-module-path %t/sdk/SubImpl.swiftmodule \
// RUN:   -emit-module-interface-path %t/sdk/SubImpl.swiftinterface \
// RUN:   -enable-library-evolution -swift-version 6
//
// RUN: %target-swift-frontend %t/modimpl.swift -sdk %t/sdk -I %t/sdk \
// RUN:   -module-name ModImpl \
// RUN:   -emit-module-path %t/sdk/ModImpl.swiftmodule \
// RUN:   -emit-module-interface-path %t/sdk/ModImpl.swiftinterface \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -public-module-name PubMod

// RUN: %target-swift-frontend %t/mod.swift -sdk %t/sdk -I %t/sdk \
// RUN:   -module-name PubMod \
// RUN:   -emit-module-path %t/sdk/PubMod.swiftmodule \
// RUN:   -emit-module-interface-path %t/sdk/PubMod.swiftinterface \
// RUN:   -enable-library-evolution -swift-version 6

// RUN: %sourcekitd-test -req=interface-gen -module PubMod -- -swift-version 6 -sdk %t/sdk -I %t/sdk -target %target-triple &> %t/PubMod.generatedinterface
// RUN: %FileCheck -input-file=%t/PubMod.generatedinterface -check-prefix INTERFACE %s

// Cursor info on the type of `ModTy.member2`, ie. `ImplTy` (see the generated `PubMod.generatedinterface`)
// RUN: %sourcekitd-test -req=interface-gen-open -module PubMod -- -swift-version 6 -sdk %t/sdk -I %t/sdk -target %target-triple \
// RUN:   == -req=cursor -pos=13:24 | %FileCheck %s --check-prefix=INTERFACE-DEF
// INTERFACE-DEF: ImplTy
// INTERFACE-DEF: PubMod


//--- common.swift
public struct CommonType {}


//--- subimpl.swift
public struct SubImplTy {}


//--- modimpl.swift
import struct Common.CommonType
import SubImpl

public struct ImplTy {
  public let member: SubImplTy
  public let member2: SubImpl.SubImplTy
}

public func fromModImpl(arg: ImplTy, arg2: ModImpl.ImplTy) {}


//--- mod.swift
@_exported import ModImpl
import struct Common.CommonType

public struct ModTy {
  public let member: ImplTy
  public let member2: ModImpl.ImplTy
}

public func fromMod(arg: ModTy, arg2: PubMod.ModTy) {}

// INTERFACE-NOT: import ModImpl
// INTERFACE: import struct Common.CommonType
// INTERFACE: import SubImpl
// INTERFACE-NOT: import
// INTERFACE: struct ImplTy
// INTERFACE:   let member: SubImplTy
// INTERFACE:   let member2: SubImplTy
// INTERFACE: struct ModTy
// INTERFACE:   let member: ImplTy
// INTERFACE:   let member2: ImplTy
// INTERFACE: func fromMod(arg: ModTy, arg2: ModTy)
// INTERFACE: func fromModImpl(arg: ImplTy, arg2: ImplTy)


//--- use.swift
import PubMod

func test() {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):3 %t/use.swift -- -swift-version 6 -sdk %t/sdk -I %t/sdk -target %target-triple %t/use.swift | %FileCheck -check-prefix=MOD %s
  fromMod()
  // MOD: fromMod
  // MOD: PubMod

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):3 %t/use.swift -- -swift-version 6 -sdk %t/sdk -I %t/sdk -target %target-triple %t/use.swift | %FileCheck -check-prefix=MODIMPL %s
  fromModImpl()
  // MODIMPL: fromModImpl
  // MODIMPL: PubMod
}
