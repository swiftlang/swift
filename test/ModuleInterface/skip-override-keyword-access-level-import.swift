// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution

// RUN: %target-swift-frontend -typecheck %t/PublicImport.swift \
// RUN:   %t/InternalImport.swift -I %t \
// RUN:   -enable-library-evolution -swift-version 5 -module-name Client \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -emit-module-interface-path %t/Client.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Client.private.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) \
// RUN:   -I %t -module-name Client
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.private.swiftinterface) \
// RUN:   -I %t -module-name Client

// RUN: %FileCheck %s < %t/Client.swiftinterface
// RUN: %FileCheck %s < %t/Client.private.swiftinterface

// REQUIRES: swift_feature_InternalImportsByDefault

//--- Lib.swift

open class Base {
  public init() {}
  open func overrideMe() { }
}

//--- PublicImport.swift

public import Lib

open class Middle: Base {
  public override init() { super.init() }
}

//--- InternalImport.swift

internal import Lib

// CHECK: open class Bottom : Client::Middle
open class Bottom: Middle {
  // CHECK: override public func overrideMe(){{$}}
  override public func overrideMe() { }
}
