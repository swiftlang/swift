// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SubheadingDeclarationFragments -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name SubheadingDeclarationFragments -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/SubheadingDeclarationFragments.symbols.json

public func foo<S>(f: @escaping () -> (), ext int: Int = 2, s: S) {}

// Subheading fragments should not contain internalParam kinds.

// CHECK-LABEL: subHeading
//              {
//                "kind": "externalParam",
// CHECK:         "spelling": "ext"
// CHECK-NEXT:  }, 
// CHECK-NEXT:  { 
// CHECK-NEXT: "kind": "text"
// CHECK-NEXT: "spelling": ": "
