// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/include)
// RUN: split-file %s %t
//
// RUN: %target-swift-frontend -emit-module -module-name a -emit-module-path %t/include/a.swiftmodule %t/a.swift -cxx-interoperability-mode=default -I %t/include
// RUN: %target-swift-frontend -typecheck -primary-file %t/b1.swift %t/b2.swift -cxx-interoperability-mode=default -I %t/include

//--- include/module.modulemap
module cxx {
  header "header.h"
  export *
}

//--- include/header.h
namespace swift {
enum class DiagID {
  x, y, z
};
}

//--- a.swift
import cxx
public struct Loc1 {}
public struct Loc2 {}
public struct Engine {
  public func diagnose(_: swift.DiagID, _: Loc1) {}
  public func diagnose(_: swift.DiagID, _: Loc2) {}
}

//--- b1.swift
import a
import cxx
private func test1(engine: Engine, loc: Loc1) {
  engine.diagnose(.y, loc)
}

//--- b2.swift
import a
import cxx
private func test2(engine: Engine, loc: Loc2, id: swift.DiagID) {
  engine.diagnose(id, loc)
}