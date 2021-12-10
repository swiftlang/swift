/// Test code completion with module aliasing
/// When -module-alias <alias_name>=<real_name> is applied, code completion should show
/// the <alias_name> as that's the name which should appear in source files including import statements,
/// decls, expressions, etc. while getting visible decls come from the module of <real_name>.
/// Below, XLogging is the alias and mapped to the real name AppleLogging. Note that the real name
/// AppleLogging should not appear in the code completion results.
///


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

class ModuleNameInClause: #^MODULE_NAME1^# {
}

// BEGIN FileLib2.swift
import XLogging

func testModuleNameInDecl() -> #^MODULE_NAME2^# {
}

// BEGIN FileLib3.swift
import XLogging

func test() {
  let x = #^EXPR^#
}

// BEGIN FileLib4.swift
import XLogging

func testModuleNameInBody() {
  XLogging.#^MODULE_NAME4^#
}

// BEGIN FileLib5.swift
import #^MODULE_NAME5^#


// EXPR: Begin completion
// EXPR-NOT: AppleLogging
// EXPR-DAG: Decl[Module]/None:                  XLogging[#Module#]; name=XLogging
// EXPR-DAG: Decl[Protocol]/OtherModule[XLogging]: Logging[#Logging#]; name=Logging
// EXPR-DAG: Decl[Struct]/OtherModule[XLogging]: Logger[#Logger#]; name=Logger
// EXPR: End completions


// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %empty-directory(%t/modules)
// RUN: %target-swift-frontend %t/FileLogging.swift -module-name AppleLogging -module-alias XLogging=AppleLogging -emit-module -o %t/modules/AppleLogging.swiftmodule

// RUN: %empty-directory(%t/output)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %t/FileLib3.swift -module-alias XLogging=AppleLogging -filecheck %raw-FileCheck -completion-output-dir %t/output -I %t/modules
