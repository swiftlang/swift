// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %empty-directory(%t/Modules)
// RUN: %target-swift-frontend -emit-module -module-name AppleLogging  -module-alias XLogging=AppleLogging -o %t/Modules %t/FileLogging.swift

// BEGIN FileLogging.swift
public struct Logger {
  public init() {}
}

public protocol Logging {
  var name: String { get }
}

public func setupLogger() -> XLogging.Logger? {
  return Logger()
}

// RUN: %empty-directory(%t/Out)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %t/FileLib1.swift -module-alias XLogging=AppleLogging  -filecheck %raw-FileCheck -completion-output-dir %t/Out -I %t/Modules

// BEGIN FileLib1.swift
import XLogging

func testModuleNameInBody() {
  #^EXPR^#
}

// EXPR: Begin completion
// EXPR-NOT: AppleLogging
// EXPR-DAG: Decl[Module]/None:                  XLogging[#Module#]; name=XLogging
// EXPR-DAG: Decl[Protocol]/OtherModule[XLogging]/Flair[RareType]: Logging[#Logging#]; name=Logging
// EXPR-DAG: Decl[Struct]/OtherModule[XLogging]: Logger[#Logger#]; name=Logger
// EXPR-DAG: Decl[FreeFunction]/OtherModule[XLogging]: setupLogger()[#Logger?#]; name=setupLogger()
 
// RUN: %empty-directory(%t/Out)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %t/FileLib2.swift -module-alias XLogging=AppleLogging  -filecheck %raw-FileCheck -completion-output-dir %t/Out -I %t/Modules

// BEGIN FileLib2.swift
import XLogging

class ModuleNameInClause: #^MODULE^# {
}

// MODULE: Begin completion
// MODULE-NOT: AppleLogging
// MODULE-DAG: Decl[Module]/None:                  XLogging[#Module#]; name=XLogging

// MODULE-DAG: Decl[Protocol]/OtherModule[XLogging]: Logging[#Logging#]; name=Logging
// MODULE-DAG: Decl[Struct]/OtherModule[XLogging]: Logger[#Logger#]; name=Logger

// RUN: %empty-directory(%t/Out)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %t/FileLib3.swift -module-alias XLogging=AppleLogging  -filecheck %raw-FileCheck -completion-output-dir %t/Out -I %t/Modules

// BEGIN FileLib3.swift
import XLogging

func testModuleNameInDecl() -> #^TYPE^# {
}

// TYPE: Begin completion
// TYPE-NOT: AppleLogging
// TYPE-DAG: Decl[Module]/None:                  XLogging[#Module#]; name=XLogging
// TYPE-DAG: Decl[Protocol]/OtherModule[XLogging]: Logging[#Logging#]; name=Logging
// TYPE-DAG: Decl[Struct]/OtherModule[XLogging]: Logger[#Logger#]; name=Logger

// RUN: %empty-directory(%t/Out)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %t/FileLib4.swift -module-alias XLogging=AppleLogging  -filecheck %raw-FileCheck -completion-output-dir %t/Out -I %t/Modules

// BEGIN FileLib4.swift
import #^IMPORT^#

// IMPORT: Begin completion
// IMPORT-NOT: AppleLogging
// IMPORT: Decl[Module]/None:                  XLogging[#Module#]; name=XLogging
