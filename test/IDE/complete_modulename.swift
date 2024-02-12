// BEGIN _Helper.swift
public struct HelperTy {}
public func helperFunc() {}

// BEGIN MyModule.swift
@_exported import _Helper

public struct MyModuleTy {}
public func myModuleFunc() {}

// BEGIN _Explicit.swift
public struct HiddenTy {}
public func hiddenFunc() {}

// BEGIN App.swift

import MyModule
import _Explicit

func test() {
 let _ = #^EXPR^#

 func test() -> #^TYPE^#
}

// EXPR: Begin completion

// EXPR-NOT: _Concurrency[#Module#]
// EXPR-NOT: SwiftShims[#Module#]
// EXPR-NOT: SwiftOnoneSupport[#Module#]
// EXPR-NOT: _Helper[#Module#]

// EXPR-DAG: Decl[Module]/None:                  swift_ide_test[#Module#]; name=swift_ide_test
// EXPR-DAG: Decl[Module]/None/IsSystem:         Swift[#Module#]; name=Swift
// EXPR-DAG: Decl[Module]/None:                  MyModule[#Module#]; name=MyModule
// EXPR-DAG: Decl[Module]/None:                  _Explicit[#Module#]; name=_Explicit
// EXPR-DAG: Decl[Struct]/OtherModule[MyModule]: MyModuleTy[#MyModuleTy#]; name=MyModuleTy
// EXPR-DAG: Decl[Struct]/OtherModule[_Explicit]: HiddenTy[#HiddenTy#]; name=HiddenTy
// EXPR-DAG: Decl[Struct]/OtherModule[_Helper]:  HelperTy[#HelperTy#]; name=HelperTy
// EXPR-DAG: Decl[FreeFunction]/OtherModule[MyModule]: myModuleFunc()[#Void#]; name=myModuleFunc()
// EXPR-DAG: Decl[FreeFunction]/OtherModule[_Explicit]: hiddenFunc()[#Void#]; name=hiddenFunc()
// EXPR-DAG: Decl[FreeFunction]/OtherModule[_Helper]: helperFunc()[#Void#]; name=helperFunc()


// TYPE: Begin completion

// TYPE-NOT: _Concurrency[#Module#]
// TYPE-NOT: SwiftShims[#Module#]
// TYPE-NOT: SwiftOnoneSupport[#Module#]
// TYPE-NOT: _Helper[#Module#]

// TYPE-DAG: Decl[Module]/None:                  swift_ide_test[#Module#]; name=swift_ide_test
// TYPE-DAG: Decl[Module]/None/IsSystem:         Swift[#Module#]; name=Swift
// TYPE-DAG: Decl[Module]/None:                  MyModule[#Module#]; name=MyModule
// TYPE-DAG: Decl[Module]/None:                  _Explicit[#Module#]; name=_Explicit
// TYPE-DAG: Decl[Struct]/OtherModule[MyModule]: MyModuleTy[#MyModuleTy#]; name=MyModuleTy
// TYPE-DAG: Decl[Struct]/OtherModule[_Explicit]: HiddenTy[#HiddenTy#]; name=HiddenTy
// TYPE-DAG: Decl[Struct]/OtherModule[_Helper]:  HelperTy[#HelperTy#]; name=HelperTy


// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %empty-directory(%t/Modules)
// RUN: %target-swift-frontend -emit-module -module-name _Helper -o %t/Modules %t/_Helper.swift
// RUN: %target-swift-frontend -emit-module -module-name MyModule -o %t/Modules %t/MyModule.swift -I %t/Modules
// RUN: %target-swift-frontend -emit-module -module-name _Explicit -o %t/Modules %t/_Explicit.swift -I %t/Modules

// RUN: %empty-directory(%t/Out)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %t/App.swift -filecheck %raw-FileCheck -completion-output-dir %t/Out -I %t/Modules
