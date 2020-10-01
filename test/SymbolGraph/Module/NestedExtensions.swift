// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/NestedExtensions/A.swift -I %t -module-name A -emit-module -emit-module-path %t/
// RUN: %target-build-swift %S/Inputs/NestedExtensions/B.swift -I %t -module-name B -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name NestedExtensions -emit-module -I %t -emit-module-path %t/

// RUN: %target-swift-symbolgraph-extract -module-name A -I %t -pretty-print -output-dir %t
// RUN: %target-swift-symbolgraph-extract -module-name B -I %t -pretty-print -output-dir %t
// RUN: %target-swift-symbolgraph-extract -module-name NestedExtensions -I %t -pretty-print -output-dir %t

// RUN: %FileCheck %s --input-file %t/B.symbols.json --check-prefix=MODULEB
// RUN: %FileCheck %s --input-file %t/B@A.symbols.json --check-prefix=MODULEBATA

// RUN: %FileCheck %s --input-file %t/NestedExtensions@A.symbols.json --check-prefix=NESTEDATA

// RUN: %FileCheck %s --input-file %t/NestedExtensions.symbols.json --check-prefix=NESTED

import A
import B

extension AStruct.BStruct {
  public struct CStruct: P {
    public func foo() -> UInt8 {
      return 0
    }
  }
}

extension AStruct.BStruct {
  public func baz() {}
}

extension AStruct.BStruct.CStruct where Thing: Equatable {
  public func baz() {}
}

// BStruct belongs to AStruct and so should only ever appear in B@A extension symbol graph files.
// MODULEB-NOT: BStruct
// MODULEBATA: "precise": "s:1A7AStructV1BE7BStructV"

// CStruct belongs to BStruct, and BStruct belongs to AStruct, so should only appear in NestedExtension@A.
// NESTED-NOT: BStruct
// NESTED-NOT: CStruct
// NESTEDATB-NOT: BStruct
// NESTEDATA: "precise": "s:1A7AStructV1BE7BStructV16NestedExtensionsE7CStructV"
