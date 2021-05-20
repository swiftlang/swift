// BEGIN MyModule.swift
public struct MyModuleTy {}
public func myModuleFunc() {}


// BEGIN _Hidden.swift
public struct HiddenTy {}
public func hiddenFunc() {}

// BEGIN App.swift

import MyModule
import _Hidden

func test() {
 let _ = #^EXPR^#

 func test() -> #^TYPE^#
}

// EXPR: Begin completion
// EXPR-NOT: _Concurrency[#Module#]
// EXPR-NOT: SwiftShims[#Module#]
// EXPR-NOT: SwiftOnoneSupport[#Module#]
// EXPR-DAG: Decl[Module]/None:                  swift_ide_test[#Module#]; name=swift_ide_test
// EXPR-DAG: Decl[Module]/None/IsSystem:         Swift[#Module#]; name=Swift
// EXPR-DAG: Decl[Module]/None:                  MyModule[#Module#]; name=MyModule
// EXPR-DAG: Decl[Struct]/OtherModule[MyModule]: MyModuleTy[#MyModuleTy#]; name=MyModuleTy
// EXPR-DAG: Decl[Struct]/OtherModule[_Hidden]:  HiddenTy[#HiddenTy#]; name=HiddenTy
// EXPR-DAG: Decl[FreeFunction]/OtherModule[MyModule]: myModuleFunc()[#Void#]; name=myModuleFunc()
// EXPR-DAG: Decl[FreeFunction]/OtherModule[_Hidden]: hiddenFunc()[#Void#]; name=hiddenFunc()
// EXPR: End completions

// TYPE: Begin completion
// TYPE-NOT: _Concurrency[#Module#]
// TYPE-NOT: SwiftShims[#Module#]
// TYPE-NOT: SwiftOnoneSupport[#Module#]
// TYPE-DAG: Decl[Module]/None:                  swift_ide_test[#Module#]; name=swift_ide_test
// TYPE-DAG: Decl[Module]/None/IsSystem:         Swift[#Module#]; name=Swift
// TYPE-DAG: Decl[Module]/None:                  MyModule[#Module#]; name=MyModule
// TYPE-DAG: Decl[Struct]/OtherModule[MyModule]: MyModuleTy[#MyModuleTy#]; name=MyModuleTy
// TYPE-DAG: Decl[Struct]/OtherModule[_Hidden]:  HiddenTy[#HiddenTy#]; name=HiddenTy
// TYPE: End completions

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %empty-directory(%t/Modules)
// RUN: %target-swift-frontend -emit-module -module-name MyModule -o %t/Modules %t/MyModule.swift
// RUN: %target-swift-frontend -emit-module -module-name _Hidden -o %t/Modules %t/_Hidden.swift

// RUN: %empty-directory(%t/Out)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %t/App.swift -filecheck %raw-FileCheck -completion-output-dir %t/Out -I %t/Modules
