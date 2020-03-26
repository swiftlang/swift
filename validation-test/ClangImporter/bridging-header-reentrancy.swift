// rdar://problem/54581756
// RUN: %empty-directory(%t)

// First set up an app-like target with the same name as a system module. Make
// sure the bridging header has a bunch of extra imports to force the
// SmallVector 'ImportedHeaderExports' in ClangImporter::Implementation to be
// reallocated.
// RUN: %target-swift-frontend -sdk "" -emit-module -o %t -import-objc-header %S/Inputs/bridging-header-reentrancy/App-Bridging-Header.h -I %S/Inputs/bridging-header-reentrancy -module-name CoincidentalNameCollision %S/Inputs/bridging-header-reentrancy/CoincidentalNameCollision.swift

// Then import that app-like target by accident in another target that also has
// a bridging header and a bunch of imports.
// RUN: %target-typecheck-verify-swift -sdk "" -I %t -import-objc-header %S/Inputs/bridging-header-reentrancy/Main-Bridging-Header.h -I %S/Inputs/bridging-header-reentrancy -verify-ignore-unknown

mainBridgingHeaderLoaded() // ok, expected
appBridgingHeaderLoaded() // ok, accidentally loaded
CNCTest() // error, accidentally shadowed
// expected-error@-1 {{use of unresolved identifier 'CNCTest'}}
