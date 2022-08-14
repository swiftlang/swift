// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -module-name B -emit-module -emit-module-path %t/mods/B.swiftmodule -experimental-allow-module-with-compiler-errors -experimental-skip-all-function-bodies -module-cache-path %t/mcp %t/B.swift
// RUN: %target-swift-frontend -module-name C -emit-module -emit-module-path %t/mods/C.swiftmodule -experimental-allow-module-with-compiler-errors -experimental-skip-all-function-bodies -I%t/mods -module-cache-path %t/mcp %t/C.swift
// RUN: %target-swift-frontend -module-name C -emit-module -emit-module-path %t/mods/C.swiftmodule -experimental-allow-module-with-compiler-errors -experimental-skip-all-function-bodies -I%t/mods -module-cache-path %t/mcp %t/C.swift

//--- mods/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -swift-version 5 -enable-library-evolution -module-name A -debug-forbid-typecheck-prefix NOTYPECHECK

import Swift
import B

public struct FromA: ProtoFromB {
  public func badMethod(arg: Int) -> Int
}

@inlinable public func topLeveLFromA() -> Int {
  let NOTYPECHECK_local = 1
  return NOTYPECHECK_local
}

//--- B.swift
public protocol ProtoFromB {
  func badMethod(arg: missing) -> Int
}

public struct StructFromB {
  public field: missing
}

//--- C.swift
import A
import B

public func fromC(a: FromA) {
  _ = a.badMethod(arg: 1)
  _ = StructFromB()
}
