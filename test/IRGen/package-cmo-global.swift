// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift %t/Core.swift \
// RUN: -module-name=Core -package-name Pkg \
// RUN: -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -Xfrontend -experimental-package-cmo \
// RUN: -enable-library-evolution -O -wmo \
// RUN: -c -g -o %t/Core.out

// %target-build-swift -module-name=Main -package-name Pkg -I%t -emit-ir -O %t/main.swift -o %t/Main.ir

// REQUIRES: asserts

//--- main.swift

import Lib

let x = ContentShapeKinds.focusEffect
let y = ContentShapeKinds._focusEffect

assert(x.Self == y.Self)

//--- Core.swift

public struct ContentShapeKinds: OptionSet {
  public var rawValue: Int
  public init(rawValue: Int) {
    self.rawValue = rawValue
  }
  public static let focusEffect = Self(rawValue: 1 << 0)
  package static let _focusEffect = Self(rawValue: 1 << 1)
}
