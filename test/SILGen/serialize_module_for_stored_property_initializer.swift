// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -c %t/inlinedStruct.swift -static -O -parse-as-library -module-name InlinedStructs -emit-module-path %t/InlinedStructs.swiftmodule -o %t/inlinedStruct.swift.o
// RUN: %target-swift-frontend -c -emit-ir -O  %t/use.swift -I %t -o %t/use.swift.ir
// RUN: cat %t/use.swift.ir | %FileCheck %s

// RUN: rm -rf %t/InlinedStructs.swiftmodule
// RUN: %target-swift-frontend -c %t/inlinedStruct.swift -O -parse-as-library -module-name InlinedStructs -emit-module-path %t/InlinedStructs.swiftmodule -o %t/inlinedStruct.swift.o
// RUN: %target-swift-frontend -c -emit-ir -O  %t/use.swift -I %t -o %t/use.swift.ir
// RUN: cat %t/use.swift.ir | %FileCheck --check-prefix=DLLIMPORT %s

// REQUIRES: OS=windows-msvc

//--- inlinedStruct.swift

@usableFromInline
struct CMSSignedData {
    @usableFromInline var field: Bool?

    @inlinable
    init(field: Bool?) {
        self.field = field
    }
}

public struct TestS {
  @usableFromInline
  let x: CMSSignedData = CMSSignedData(field: false)

  @inlinable
  public init() {  }

   @inlinable
  public var field: Bool {
    return x.field!
  }
}

//--- use.swift

import InlinedStructs

public struct TestTwo {
  let field: TestS = TestS()
}

public func testTwo() -> Bool {
  let x = TestTwo()
  return x.field.field
}

// Ensure that the variable initialization expression is not dllimported on Windows.
// CHECK: declare swiftcc i8 @"$s14InlinedStructs5TestSV1xAA13CMSSignedDataVvpfi"()
// DLLIMPORT: declare dllimport swiftcc i8 @"$s14InlinedStructs5TestSV1xAA13CMSSignedDataVvpfi"()
