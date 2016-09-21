// RUN: %target-swift-frontend -emit-silgen -I %S/../IDE/Inputs/custom-modules %s 2>&1 | %FileCheck --check-prefix=SIL %s
// REQUIRES: objc_interop

import ImportAsMember.C

// SIL-LABEL: sil {{.*}}readSemiModularPowerSupply{{.*}}
public func readSemiModularPowerSupply() -> CCPowerSupply {
  // TODO: actual body
  // FIXME: this asserts
  return CCPowerSupply.semiModular
}

