// RUN: %swift -target thumbv7-unknown-windows-msvc -parse-stdlib -parse-as-library -I %S/Inputs/usr/include -module-name Swift -S -emit-ir -o - %s | %FileCheck %s
// RUN: %swift -target thumbv7-unknown-linux-gnueabihf -parse-stdlib -parse-as-library -I %S/Inputs/usr/include -module-name Swift -S -emit-ir -o - %s | %FileCheck %s
// RUN: %swift -target thumbv7-unknown-linux-gnueabi -Xcc -mfloat-abi=hard -parse-stdlib -parse-as-library -I %S/Inputs/usr/include -module-name Swift -S -emit-ir -o - %s | %FileCheck %s

// REQUIRES: CODEGENERATOR=ARM

struct Float {
  let _value: Builtin.FPIEEE32
}

typealias CFloat = Float
typealias Void = ()

import SRoA

func f() -> Float {
  return add(frand(), frand())
}

// CHECK: call arm_aapcs_vfpcc float @add

