// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: not --crash %target-swift-frontend -emit-ir -I %t/Inputs %t/main.swift
// REQUIRES: OS=macosx

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

//--- main.swift
import ObjCLib
protocol a {
  associatedtype b
}
protocol c {
}
protocol d {
}
extension Int: c {
}
struct h<e>: d {
}
struct f<e>: a {
  typealias b = Int
}
func g<e: a>(_: e) -> some d where e.b: c {
  if #available(macOS 9, *) {
    return h<e>()
  }
  return g(f<Box<Base>>())
}
