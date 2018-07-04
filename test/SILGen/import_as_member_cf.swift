// RUN: %target-swift-emit-silgen -enable-sil-ownership -I %S/../IDE/Inputs/custom-modules %s -enable-objc-interop -sdk %S/Inputs 2>&1 | %FileCheck --check-prefix=SIL %s

import ImportAsMember.C

// SIL-LABEL: sil {{.*}}readSemiModularPowerSupply{{.*}}
public func readSemiModularPowerSupply() -> CCPowerSupply {
  // TODO: actual body
  // FIXME: this asserts
  return CCPowerSupply.semiModular
}

