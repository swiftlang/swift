// RUN: %target-swift-frontend -enable-objc-interop -emit-module %s -sdk %S/Inputs -module-name HasSubmodule -I %S/Inputs/custom-modules -o %t

@_exported import HasSubmodule
@_exported import HasSubmodule.Submodule

func useAnOperator() -> Int {
  var x : Int
  x = 1 + 2           // Forces a lookup of precedence groups
  return x
}
