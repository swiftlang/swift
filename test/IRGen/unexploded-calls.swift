// RUN: %swift -target thumbv7-unknown-windows-msvc -parse-stdlib -parse-as-library -I %S/Inputs/usr/include -module-name Swift -S -emit-ir -o - %s | %FileCheck %s
// RUN: %swift -target thumbv7-unknown-linux-gnueabihf -parse-stdlib -parse-as-library -I %S/Inputs/usr/include -module-name Swift -S -emit-ir -o - %s | %FileCheck %s
// RUN: %swift -target thumbv7-unknown-linux-gnueabi -Xcc -mfloat-abi=hard -parse-stdlib -parse-as-library -I %S/Inputs/usr/include -module-name Swift -S -emit-ir -o - %s | %FileCheck %s

struct Float {
  let _value: Builtin.FPIEEE32
}

typealias CFloat = Float
typealias Void = ()

import SRoA

public func g(_ s : S) {
  return f(s)
}

// CHECK: define {{.*}}swiftcc void @_T0s1gySC1SVF(float, float) {{.*}}{
// CHECK: entry:
// CHECK:   %coerced-arg.coerced = alloca %TSC1SV, align 4
// CHECK:   %2 = bitcast %TSC1SV* %coerced-arg.coerced to i8*
// CHECK:   %coerced-arg.coerced.f = getelementptr inbounds %TSC1SV, %TSC1SV* %coerced-arg.coerced, i32 0, i32 0
// CHECK:   %coerced-arg.coerced.f._value = getelementptr inbounds %TSf, %TSf* %coerced-arg.coerced.f, i32 0, i32 0
// CHECK:   store float %0, float* %coerced-arg.coerced.f._value, align 4
// CHECK:   %coerced-arg.coerced.g = getelementptr inbounds %TSC1SV, %TSC1SV* %coerced-arg.coerced, i32 0, i32 1
// CHECK:   %coerced-arg.coerced.g._value = getelementptr inbounds %TSf, %TSf* %coerced-arg.coerced.g, i32 0, i32 0
// CHECK:   store float %1, float* %coerced-arg.coerced.g._value, align 4
// CHECK:   %3 = bitcast %TSC1SV* %coerced-arg.coerced to %struct.S*
// CHECK:   %4 = load %struct.S, %struct.S* %3, align 4
// CHECK:   call void @f(%struct.S %4)
// CHECK: }

