// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -parse-as-library -enable-library-evolution %t/module.swift -emit-module-path=%t/Module.swiftmodule -module-name=Module
// RUN: %target-swift-frontend -emit-sil -o /dev/null -parse-as-library %t/main.swift -I%t -enable-experimental-feature SymbolLinkageMarkers

// REQUIRES: swift_feature_SymbolLinkageMarkers

// Check that this compiles successfully

//--- module.swift

public struct X: ~Copyable {
  public init() {}
  deinit {
    print("deinit")
  }
}

//--- main.swift

import Module

@_section("__TEXT,__mysection")
var g: () -> () = { _ = X() }
