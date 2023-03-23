/// Test code completion with module aliasing
/// When -module-alias <alias_name>=<real_name> is applied, code completion should show
/// the <alias_name> as that's the name which should appear in source files including import statements,
/// decls, expressions, etc. while getting visible decls come from the module of <real_name>.
/// Below, XLogging is the alias and mapped to the real name AppleLogging. Note that the real name
/// AppleLogging should not appear in the code completion results.
///
// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend %t/FileLogging.swift -module-name AppleLogging -module-alias XLogging=AppleLogging -emit-module -o %t/AppleLogging.swiftmodule

// RUN: %target-swift-ide-test -code-completion -code-completion-token=MODULE_NAME -source-filename %t/FileLib1.swift -module-alias XLogging=AppleLogging -I %t > %t/result1.txt
// RUN: %FileCheck %s -check-prefix CHECK12 < %t/result1.txt

// RUN: %target-swift-ide-test -code-completion -code-completion-token=MODULE_NAME -source-filename %t/FileLib2.swift -module-alias XLogging=AppleLogging -I %t > %t/result2.txt
// RUN: %FileCheck %s -check-prefix CHECK12 < %t/result2.txt

// CHECK12: found code completion token MODULE_NAME
// CHECK12-NOT: AppleLogging
// CHECK12-DAG: Decl[Module]/None:                  XLogging[#Module#]; name=XLogging
// CHECK12-DAG: Decl[Protocol]/OtherModule[XLogging]: Logging[#Logging#]; name=Logging
// CHECK12-DAG: Decl[Struct]/OtherModule[XLogging]: Logger[#Logger#]; name=Logger

// RUN: %target-swift-ide-test -code-completion -code-completion-token=MODULE_NAME -source-filename %t/FileLib3.swift -module-alias XLogging=AppleLogging -I %t > %t/result3.txt
// RUN: %FileCheck %s -check-prefix CHECK3 < %t/result3.txt

// CHECK3: found code completion token MODULE_NAME
// CHECK3-NOT: AppleLogging
// CHECK3-DAG: Decl[Module]/None:                  XLogging[#Module#]; name=XLogging
// CHECK3-DAG: Decl[Protocol]/OtherModule[XLogging]/Flair[RareType]: Logging[#Logging#]; name=Logging
// CHECK3-DAG: Decl[Struct]/OtherModule[XLogging]: Logger[#Logger#]; name=Logger
// CHECK3-DAG: Decl[FreeFunction]/OtherModule[XLogging]: setupLogger()[#Logger?#]; name=setupLogger()


// RUN: %target-swift-ide-test -code-completion -code-completion-token=MODULE_NAME -source-filename %t/FileLib4.swift -module-alias XLogging=AppleLogging -I %t > %t/result4.txt
// RUN: %FileCheck %s -check-prefix CHECK4 < %t/result4.txt

// CHECK4: found code completion token MODULE_NAME
// CHECK4-NOT: AppleLogging
// CHECK4-DAG: Decl[Protocol]/OtherModule[XLogging]/Flair[RareType]: Logging[#Logging#]; name=Logging
// CHECK4-DAG: Decl[Struct]/OtherModule[XLogging]: Logger[#Logger#]; name=Logger
// CHECK4-DAG: Decl[FreeFunction]/OtherModule[XLogging]: setupLogger()[#Logger?#]; name=setupLogger()

/// In the following, the module alias name should be shown as a module that can be imported instead of the real name
///
// RUN: %target-swift-ide-test -code-completion -code-completion-token=MODULE_NAME -source-filename %t/FileLib5.swift -module-alias XLogging=AppleLogging -I %t > %t/result5.txt
// RUN: %FileCheck %s -check-prefix CHECK5 < %t/result5.txt

/// In search paths, only AppleLogging.swiftmodule exists, but when `-module-alias XLogging=AppleLogging` is passed,
/// we want to only show XLogging as an option to import, not AppleLogging
// CHECK5: found code completion token MODULE_NAME
// CHECK5-NOT: AppleLogging
// CHECK5: Decl[Module]/None:                  XLogging[#Module#]; name=XLogging


// RUN: %target-swift-ide-test -code-completion -code-completion-token=MODULE_NAME -source-filename %t/FileLib5.swift -I %t > %t/result6.txt
// RUN: %FileCheck %s -check-prefix CHECK6 < %t/result6.txt

/// In search paths, only AppleLogging.swiftmodule exists, and no module aliasing option is passed, so
/// just show AppleLogging as one of the modules that can be imported
// CHECK6: found code completion token MODULE_NAME
// CHECK6-NOT: XLogging
// CHECK6: Decl[Module]/None:                  AppleLogging[#Module#]; name=AppleLogging


// BEGIN FileLogging.swift
public struct Logger {
  public init() {}
}

public protocol Logging {
  var content: String { get }
}

public func setupLogger() -> XLogging.Logger? {
  return Logger()
}

// BEGIN FileLib1.swift
import XLogging

class ModuleNameInClause: #^MODULE_NAME^# {
}

// BEGIN FileLib2.swift
import XLogging

func testModuleNameInDecl() -> #^MODULE_NAME^# {
}

// BEGIN FileLib3.swift
import XLogging

func testModuleNameInBody() {
  #^MODULE_NAME^#
}

// BEGIN FileLib4.swift
import XLogging

func testModuleNameInBody() {
  XLogging.#^MODULE_NAME^#
}

// BEGIN FileLib5.swift
import #^MODULE_NAME^#

