// RUN: %swift -i %s | FileCheck %s

import Builtin
import swift

printf("%x\n", Int32(Builtin.bitcast_FPIEEE32_Int32(Float32(1.0).value))) // CHECK: {{^}}3f800000{{$}}
printf("%x\n", Int64(Builtin.bitcast_FPIEEE64_Int64(Float64(1.0).value))) // CHECK: {{^}}3ff0000000000000{{$}}
printf("%x\n", Int128(Builtin.zext_Int80_Int128(Builtin.bitcast_FPIEEE80_Int80(Float80(1.0).value)))) // CHECK: {{^}}3fff8000000000000000{{$}}

