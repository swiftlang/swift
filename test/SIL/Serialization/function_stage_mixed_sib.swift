// RUN: %empty-directory(%t)
// RUN: split-file %s %t
//
// A whole-module compile can mix source files with SIBs. A SIB records the
// stage floor its producer committed to, but that floor describes the SIB,
// not the module being compiled. When the same compile also generates SIL
// from source, that SIL is raw: the module floor must stay Raw so that the
// mandatory pipeline still runs over it. Each SIB function keeps the stage it
// was recorded at.
//
// RUN: %target-swift-frontend -emit-sib %t/lib.swift -module-name M -parse-as-library -o %t/lib.sib
// RUN: %target-swift-frontend -emit-sibgen %t/user.swift -module-name M -parse-as-library -o %t/user-raw.sib
//
// Source plus a canonical SIB.
// RUN: %target-swift-frontend -emit-sil %t/user.swift %t/lib.sib -module-name M -parse-as-library -o - | %FileCheck %s --check-prefix=MIXED
// RUN: %target-swift-frontend -c %t/user.swift %t/lib.sib -module-name M -parse-as-library -o %t/mixed.o
//
// A raw SIB plus a canonical SIB, in either order.
// RUN: %target-swift-frontend -c %t/lib.sib %t/user-raw.sib -module-name M -parse-as-library -o %t/sibs1.o
// RUN: %target-swift-frontend -c %t/user-raw.sib %t/lib.sib -module-name M -parse-as-library -o %t/sibs2.o
//
// The mandatory diagnostics still run over the source file.
// RUN: not %target-swift-frontend -c %t/uninit.swift %t/lib.sib -module-name M -parse-as-library -o /dev/null 2>&1 | %FileCheck %s --check-prefix=DI
//
// A canonical SIB on its own is still an already-canonical input.
// RUN: %target-swift-frontend -emit-sil %t/lib.sib -module-name M -parse-as-library -o - | %FileCheck %s --check-prefix=SIBONLY

// MIXED: sil_stage canonical
// MIXED-NOT: [unknown]
// MIXED-LABEL: sil @$s1M5afuncyS2iF :
// MIXED-NOT: [unknown]
// MIXED: } // end sil function '$s1M5afuncyS2iF'

// DI: error: constant 'x' used before being initialized

// SIBONLY: sil_stage canonical
// SIBONLY-LABEL: sil @$s1M5bfuncSiyF :

//--- lib.swift
public func bfunc() -> Int { return 42 }

//--- user.swift
public func afunc(_ n: Int) -> Int {
  var y = n
  y += 1
  return y
}

//--- uninit.swift
public func ufunc() -> Int {
  let x: Int
  return x
}
