// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t -I %S/Inputs/custom-modules) -print-module -source-filename %s -module-to-print=ImportAsMember.C -always-argument-labels > %t.printed.C.txt

// REQUIRES: objc_interop

// RUN: %FileCheck %s -check-prefix=PRINTC -strict-whitespace < %t.printed.C.txt

// PRINTC:      extension CCPowerSupply {
// PRINTC-NEXT:   /*not inherited*/ init(watts watts: Double)
// PRINTC-NEXT:   class let semiModular: CCPowerSupply!
// PRINTC-NEXT:   /*not inherited*/ init(dangerous dangerous: ())
// PRINTC-NEXT:   class let defaultPower: Double
// PRINTC-NEXT:   class let AC: CCPowerSupply
// PRINTC-NEXT:   class let DC: CCPowerSupply?
// PRINTC-NEXT: }

// PRINTC:      extension CCRefrigerator {
// PRINTC-NEXT:   /*not inherited*/ init(powerSupply power: CCPowerSupply)
// PRINTC-NEXT:   func open()
// PRINTC-NEXT:   var powerSupply: CCPowerSupply
// PRINTC-NEXT: }

// PRINTC:      extension CCMutableRefrigerator {
// PRINTC-NEXT:   /*not inherited*/ init(powerSupply power: CCPowerSupply)
// PRINTC-NEXT: }

// RUN: %target-typecheck-verify-swift -I %S/Inputs/custom-modules

import ImportAsMember.C

let powerSupply = CCPowerSupply(watts: 500.0)
let refrigerator = CCRefrigerator(powerSupply: powerSupply)
refrigerator.open();
refrigerator.powerSupply = powerSupply
