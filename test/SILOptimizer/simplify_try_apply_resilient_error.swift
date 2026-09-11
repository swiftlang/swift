// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Lib built with library evolution: `NoCases` can gain cases in a future version.
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -swift-version 6 \
// RUN:   -module-name Lib %t/Lib.swift -emit-module-path %t/Lib.swiftmodule
// RUN: %target-swift-frontend -emit-sil -O -swift-version 6 -I %t -module-name Client \
// RUN:   %t/Client.swift | %FileCheck %s -check-prefix=RESILIENT

/// The same Lib without library evolution: `NoCases` is exhaustive, so it stays uninhabited.
// RUN: %target-swift-frontend -emit-module -swift-version 6 \
// RUN:   -module-name Lib %t/Lib.swift -emit-module-path %t/Lib.swiftmodule
// RUN: %target-swift-frontend -emit-sil -O -swift-version 6 -I %t -module-name Client \
// RUN:   %t/Client.swift | %FileCheck %s -check-prefix=EXHAUSTIVE

//--- Lib.swift

public enum NoCases: Error {}

public func mightThrow<R>(_ body: () throws(NoCases) -> R) throws(NoCases) -> R {
  try body()
}

//--- Client.swift

import Lib

// RESILIENT-LABEL: sil @$s6Client4callSiy3Lib7NoCasesOYKF :
// RESILIENT:         try_apply
// RESILIENT:       } // end sil function '$s6Client4callSiy3Lib7NoCasesOYKF'

// EXHAUSTIVE-LABEL: sil @$s6Client4callSiy3Lib7NoCasesOYKF :
// EXHAUSTIVE:         apply [nothrow]
// EXHAUSTIVE-NOT:     try_apply
// EXHAUSTIVE:       } // end sil function '$s6Client4callSiy3Lib7NoCasesOYKF'
public func call() throws(NoCases) -> Int {
  try mightThrow { 42 }
}
