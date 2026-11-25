// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -emit-module-path %t/print_module_abi_attr.swiftmodule %s
// RUN: %target-swift-ide-test -print-module -print-interface -no-empty-line-between-members -module-to-print=print_module_abi_attr -I %t -source-filename=%s | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -print-interface-doc -no-empty-line-between-members -module-to-print=print_module_abi_attr -I %t -source-filename=%s | %FileCheck %s -check-prefix=DOC

public struct S {
  @abi(var abiStored: Int)
  public var apiStored: Int

  @abi(func abiMethod())
  public func apiMethod() {}
}

// CHECK-LABEL: public struct S {
// CHECK-NEXT:    @abi(var abiStored: Int)
// CHECK-NEXT:    public var apiStored: Int
// CHECK-NEXT:    @abi(func abiMethod())
// CHECK-NEXT:    public func apiMethod()
// CHECK-NEXT:  }

// DOC-LABEL: struct S {
// DOC-NEXT:    @abi(var abiStored: Int)
// DOC-NEXT:    var apiStored: Int
// DOC-NEXT:    @abi(func abiMethod())
// DOC-NEXT:    func apiMethod()
// DOC-NEXT:  }
