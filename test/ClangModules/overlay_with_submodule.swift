// RUN: %target-swift-frontend -emit-module %s -sdk %S/Inputs -module-name HasSubmodule  -I %S/Inputs/custom-modules -o %t

// REQUIRES: objc_interop

@_exported import HasSubmodule
@_exported import HasSubmodule.Submodule

func useAnOperator() -> Int {
  var x : Int
  x = 1 + 2           // Forces a lookup of precedence groups
  return x
}
