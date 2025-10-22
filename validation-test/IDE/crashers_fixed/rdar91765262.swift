// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ImportPath)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -disable-availability-checking -emit-module %t/Lib.swift -o %t/ImportPath/Lib.swiftmodule -emit-module-interface-path %t/ImportPath/Lib.swiftinterface

// BEGIN Lib.swift

// Proto and Class have a circular supertype relationship.

public protocol Proto : Class {}
public class Class : Proto {}

// BEGIN test.swift

import Lib

// RUN: %empty-directory(%t/completion-cache)
// RUN: %target-swift-ide-test -code-completion -source-filename %t/test.swift -code-completion-token COMPLETE -I %t/ImportPath

func test() -> Proto {
  return #^COMPLETE^#
}
