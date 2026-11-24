// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -module-name Lib -o %t/Lib.swiftmodule %t/Lib.swift
// RUN: not --crash %target-swift-frontend -emit-ir -I %t -I %t/Inputs %t/main.swift
// REQUIRES: objc_interop

//--- Inputs/module.modulemap
module ObjCLib {
  header "ObjCLib.h"
}

//--- Inputs/ObjCLib.h
__attribute__((objc_root_class))
@interface Base
@end

@interface Box<T> : Base
@end

//--- Lib.swift
public protocol Seq {
  associatedtype Element
}
public protocol P {}
public protocol Q {}

public struct Wrapper<T>: Q {
  public init() {}
}

extension Seq where Element: P {
  public func wrapped() -> some Q { Wrapper<Self>() }
}

//--- main.swift
import Lib
import ObjCLib

struct MapSeq<Input, Output>: Seq {
  typealias Element = Output
}

public protocol MakeQ {
  associatedtype Out: Q
  func make() -> Out
}

struct S<T>: P {}

struct R: MakeQ {
  func make() -> some Q {
    MapSeq<Box<Base>, S<Base>>().wrapped()
  }
}

public func makeProvider() -> any MakeQ { R() }
