// RUN: %target-typecheck-verify-swift -enable-experimental-feature SymbolLinkageMarkers -parse-as-library

// REQUIRES: swift_feature_SymbolLinkageMarkers

@_section("__TEXT,__mysection") // expected-warning {{'@_section' has been renamed to '@section'}}
let renamedSection: Int = 42
