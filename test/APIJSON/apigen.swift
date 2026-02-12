// REQUIRES: objc_interop, OS=macosx
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) %s -typecheck -parse-as-library -emit-module-interface-path %t/MyModule.swiftinterface -enable-library-evolution -module-name MyModule -package-name MyModule -swift-version 5 -library-level api
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) %s -typecheck -parse-as-library -emit-module-interface-path %t/MyModule.swiftinterface -enable-library-evolution -module-name MyModule -package-name MyModule -swift-version 5 -emit-api-descriptor-path %t/api.json -library-level api
// RUN: %validate-json %t/api.json | %FileCheck %s

import Foundation

@available(macOS 10.13, *)
public class Test : NSObject {
  @objc public func method1() {}
  @available(macOS 10.14, *)
  @objc public class func method2() {}
  public func nonObjc() {}
}

@available(macOS 10.13, *)
public class Derived : Test {
  @objc public override func method1() {}
  public override func nonObjc() {}

  private var _readOnly : Int
  package init(readOnly: Int) {
    _readOnly = readOnly
  }
  public package(set) var readOnly : Int {
    get { _readOnly }
    set { _readOnly = newValue }
  }
}

// Member declarations inside a `public extension` are public implicitly
public extension Derived {
  func inheritlyPublic() {}
  private func privateFunc() {}
}

// Not derived from NSObject. ObjC metadata is still emitted but not exported.
// Ignore those metadata for now.
public class Test2 {}

// Test initializers.
public class Test3 : NSObject {
  @objc public override init() {}
  @objc public init(fromInt number: Int) {}
  @objc public convenience init(_ number: Float) {
    self.init()
  }
}

@available(macOS 10.13, *)
package func myFunction() -> Int {
  return 0
}

@available(macOS, obsoleted: 10.9)
public func myFunction1() {}

@available(*, unavailable)
public func myFunction2() {}

package func packageFunction() {}

internal func internalFunction() {}

private func privateFunction() {}

fileprivate func fileprivateFunction() {}

func implicitInternalFunction() {}

@available(macOS 10.13, *)
public var myGlobalVar: Int = 42

// CHECK:      "target"
// CHECK-NEXT: "globals": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule10myFunctionSiyF",
// CHECK-NEXT:     "access": "private",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule11myFunction1yyF",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "obsoleted": "10.9"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule11myFunction2yyF",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "unavailable": true
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule11myGlobalVarSivM",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule11myGlobalVarSivg",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule11myGlobalVarSivs",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule11myGlobalVarSivx",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule11myGlobalVarSivxTwc",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule15packageFunctionyyF",
// CHECK-NEXT:     "access": "private",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule4TestC7method1yyFTj",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule4TestC7method1yyFTq",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule4TestC7method2yyFZTj",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.14"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule4TestC7method2yyFZTq",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.14"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule4TestC7nonObjcyyFTj",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule4TestC7nonObjcyyFTq",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule4TestCACycfC",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule4TestCACycfc",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule4TestCMa",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule4TestCMn",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule4TestCMo",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule4TestCMu",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule4TestCN",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule4TestCfD",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test2CMa",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test2CMm",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test2CMn",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test2CMo",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:      "name": "_$s8MyModule5Test2CMu",
// CHECK-NEXT:      "access": "public",
// CHECK-NEXT:      "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:      "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test2CN",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test2CfD",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test2Cfd",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test3C7fromIntACSi_tcfC",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test3C7fromIntACSi_tcfCTj",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test3C7fromIntACSi_tcfCTq",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test3C7fromIntACSi_tcfc",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test3CACycfC",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test3CACycfc",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test3CMa",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test3CMn",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test3CMo",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test3CMu",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test3CN",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test3CfD",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test3CyACSfcfC",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule5Test3CyACSfcfc",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedC15inheritlyPublicyyF",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedC8readOnlyACSi_tcfC",
// CHECK-NEXT:     "access": "private",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedC8readOnlyACSi_tcfCTj",
// CHECK-NEXT:     "access": "private",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedC8readOnlyACSi_tcfCTq",
// CHECK-NEXT:     "access": "private",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedC8readOnlyACSi_tcfc",
// CHECK-NEXT:     "access": "private",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedC8readOnlySivMTj",
// CHECK-NEXT:     "access": "private",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedC8readOnlySivMTq",
// CHECK-NEXT:     "access": "private",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedC8readOnlySivgTj",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedC8readOnlySivgTq",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedC8readOnlySivpMV",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedC8readOnlySivsTj",
// CHECK-NEXT:     "access": "private",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedC8readOnlySivsTq",
// CHECK-NEXT:     "access": "private",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedC8readOnlySivxTj",
// CHECK-NEXT:     "access": "private",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedC8readOnlySivxTjTwc",
// CHECK-NEXT:     "access": "private",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedC8readOnlySivxTq",
// CHECK-NEXT:     "access": "private",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedCACycfC",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedCACycfc",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedCMa",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedCMn",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedCMo",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedCMu",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedCN",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_$s8MyModule7DerivedCfD",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13"
// CHECK-NEXT:   }
// CHECK-NEXT: ],
// CHECK-NEXT: "interfaces": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_TtC8MyModule4Test",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13",
// CHECK-NEXT:     "super": "NSObject",
// CHECK-NEXT:     "instanceMethods": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "name": "method1",
// CHECK-NEXT:         "access": "public",
// CHECK-NEXT:         "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:         "introduced": "10.13"
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "name": "init",
// CHECK-NEXT:         "access": "public",
// CHECK-NEXT:         "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:         "introduced": "10.13"
// CHECK-NEXT:       }
// CHECK-NEXT:     ],
// CHECK-NEXT:     "classMethods": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "name": "method2",
// CHECK-NEXT:         "access": "public",
// CHECK-NEXT:         "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:         "introduced": "10.14"
// CHECK-NEXT:       }
// CHECK-NEXT:     ]
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_TtC8MyModule5Test3",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "super": "NSObject",
// CHECK-NEXT:     "instanceMethods": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "name": "init",
// CHECK-NEXT:         "access": "public",
// CHECK-NEXT:         "file": "SOURCE_DIR/test/APIJSON/apigen.swift"
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "name": "initFromInt:",
// CHECK-NEXT:         "access": "public",
// CHECK-NEXT:         "file": "SOURCE_DIR/test/APIJSON/apigen.swift"
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "name": "init:",
// CHECK-NEXT:         "access": "public",
// CHECK-NEXT:         "file": "SOURCE_DIR/test/APIJSON/apigen.swift"
// CHECK-NEXT:       }
// CHECK-NEXT:     ],
// CHECK-NEXT:     "classMethods": []
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "name": "_TtC8MyModule7Derived",
// CHECK-NEXT:     "access": "public",
// CHECK-NEXT:     "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:     "linkage": "exported",
// CHECK-NEXT:     "introduced": "10.13",
// CHECK-NEXT:     "super": "_TtC8MyModule4Test",
// CHECK-NEXT:     "instanceMethods": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "name": "method1",
// CHECK-NEXT:         "access": "public",
// CHECK-NEXT:         "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:         "introduced": "10.13"
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "name": "init",
// CHECK-NEXT:         "access": "public",
// CHECK-NEXT:         "file": "SOURCE_DIR/test/APIJSON/apigen.swift",
// CHECK-NEXT:         "introduced": "10.13"
// CHECK-NEXT:       }
// CHECK-NEXT:     ],
// CHECK-NEXT:     "classMethods": []
// CHECK-NEXT:   }
// CHECK-NEXT: ],
// CHECK-NEXT: "categories": [],
// CHECK-NEXT: "version": "1.0"
