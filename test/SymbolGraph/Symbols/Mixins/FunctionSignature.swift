// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name FunctionSignature -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name FunctionSignature -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/FunctionSignature.symbols.json --check-prefix=FUNC
// RUN: %FileCheck %s --input-file %t/FunctionSignature.symbols.json --check-prefix=INIT
// RUN: %FileCheck %s --input-file %t/FunctionSignature.symbols.json --check-prefix=SUBSCRIPT
// RUN: %FileCheck %s --input-file %t/FunctionSignature.symbols.json --check-prefix=FUNC2

public struct MyStruct {
  public init(_ noext: Int, ext int: Int) {}
    
// INIT-LABEL: "precise": "s:17FunctionSignature8MyStructV_3extACSi_Sitcfc",
// INIT: "name": "noext"
// INIT-NOT: "internalName": "noext"
// INIT-NEXT: declarationFragments

// INIT: "kind": "identifier"
// INIT-NEXT: "spelling": "noext"
// INIT: "kind": "text"
// INIT-NEXT: "spelling": ": "
// INIT: "kind": "typeIdentifier"
// INIT-NEXT: "spelling": "Int"
// INIT-NEXT: "preciseIdentifier": "s:Si"

// INIT: "name": "ext"
// INIT-NEXT: "internalName": "int"
// INIT-NEXT: declarationFragments

// INIT: "kind": "identifier"
// INIT-NEXT: "spelling": "int"
// INIT: "kind": "text"
// INIT-NEXT: "spelling": ": "
// INIT: "kind": "typeIdentifier"
// INIT-NEXT: "spelling": "Int"
// INIT-NEXT: "preciseIdentifier": "s:Si"
    
  public subscript(_ noext: Int, ext int: Int) -> String {
    get { return "OK" }
    set { }
  }
    
// SUBSCRIPT-LABEL: "precise": "s:17FunctionSignature8MyStructV_3extSSSi_Sitcip",
// SUBSCRIPT: "name": "noext"
// SUBSCRIPT-NOT: "internalName": "noext"
// SUBSCRIPT-NEXT: declarationFragments

// SUBSCRIPT: "kind": "identifier"
// SUBSCRIPT-NEXT: "spelling": "noext"
// SUBSCRIPT: "kind": "text"
// SUBSCRIPT-NEXT: "spelling": ": "
// SUBSCRIPT: "kind": "typeIdentifier"
// SUBSCRIPT-NEXT: "spelling": "Int"
// SUBSCRIPT-NEXT: "preciseIdentifier": "s:Si"

// SUBSCRIPT: "name": "ext"
// SUBSCRIPT-NEXT: "internalName": "int"
// SUBSCRIPT-NEXT: declarationFragments

// SUBSCRIPT: "kind": "identifier"
// SUBSCRIPT-NEXT: "spelling": "int"
// SUBSCRIPT: "kind": "text"
// SUBSCRIPT-NEXT: "spelling": ": "
// SUBSCRIPT: "kind": "typeIdentifier"
// SUBSCRIPT-NEXT: "spelling": "Int"
// SUBSCRIPT-NEXT: "preciseIdentifier": "s:Si"

// SUBSCRIPT: returns
// SUBSCRIPT: "kind": "typeIdentifier"
// SUBSCRIPT-NEXT: "spelling": "String"
// SUBSCRIPT-NEXT: "preciseIdentifier": "s:SS"
    
  public func foo(_ noext: Int, ext int: Int) -> String {
    return "OK"
  }
    
// FUNC-LABEL: "precise": "s:17FunctionSignature8MyStructV3foo_3extSSSi_SitF",
// FUNC: "name": "noext"
// FUNC-NOT: "internalName": "noext"
// FUNC-NEXT: declarationFragments

// FUNC: "kind": "identifier"
// FUNC-NEXT: "spelling": "noext"
// FUNC: "kind": "text"
// FUNC-NEXT: "spelling": ": "
// FUNC: "kind": "typeIdentifier"
// FUNC-NEXT: "spelling": "Int"
// FUNC-NEXT: "preciseIdentifier": "s:Si"

// FUNC: "name": "ext"
// FUNC-NEXT: "internalName": "int"
// FUNC-NEXT: declarationFragments

// FUNC: "kind": "identifier"
// FUNC-NEXT: "spelling": "int"
// FUNC: "kind": "text"
// FUNC-NEXT: "spelling": ": "
// FUNC: "kind": "typeIdentifier"
// FUNC-NEXT: "spelling": "Int"
// FUNC-NEXT: "preciseIdentifier": "s:Si"

// FUNC: returns
// FUNC: "kind": "typeIdentifier"
// FUNC-NEXT: "spelling": "String"
// FUNC-NEXT: "preciseIdentifier": "s:SS"
    
  public func bar(_: Int, ext _: Int) -> Void {}
      
// FUNC2-LABEL: "precise": "s:17FunctionSignature8MyStructV3bar_3extySi_SitF",
// FUNC2: "name": "_"
// FUNC2-NOT: "internalName": "_"
// FUNC2-NEXT: declarationFragments

// FUNC2: "kind": "identifier"
// FUNC2-NEXT: "spelling": "_"
// FUNC2: "kind": "text"
// FUNC2-NEXT: "spelling": ": "
// FUNC2: "kind": "typeIdentifier"
// FUNC2-NEXT: "spelling": "Int"
// FUNC2-NEXT: "preciseIdentifier": "s:Si"

// FUNC2: "name": "ext"
// FUNC2-NEXT: "internalName": "_"
// FUNC2-NEXT: declarationFragments

// FUNC2: "kind": "identifier"
// FUNC2-NEXT: "spelling": "_"
// FUNC2: "kind": "text"
// FUNC2-NEXT: "spelling": ": "
// FUNC2: "kind": "typeIdentifier"
// FUNC2-NEXT: "spelling": "Int"
// FUNC2-NEXT: "preciseIdentifier": "s:Si"

// FUNC2: returns
// FUNC2: "kind": "typeIdentifier"
// FUNC2-NEXT: "spelling": "Void"
// FUNC2-NEXT: "preciseIdentifier": "s:s4Voida"
    
}
