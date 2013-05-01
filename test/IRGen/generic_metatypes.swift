// RUN: %swift -emit-llvm -triple=x86_64-apple-darwin10 %s | FileCheck %s

import Builtin

// CHECK: define %swift.type* @_T17generic_metatypes13genericTypeofU__FT1xQ__MQ_(%swift.opaque*, %swift.type* [[TYPE:%.*]])
func genericTypeof<T>(x:T) -> T.metatype {
  // CHECK: [[X:%.*]] = call %swift.opaque* %allocateBuffer([24 x i8]* {{%.*}}, %swift.type* [[TYPE]])
  // CHECK: [[TYPEOF_WITNESS_ADDR:%.*]] = getelementptr inbounds i8** {{%.*}}, i32 12
  // CHECK: [[TYPEOF_WITNESS:%.*]] = load i8** [[TYPEOF_WITNESS_ADDR]], align 8
  // CHECK: %typeof = bitcast i8* [[TYPEOF_WITNESS]] to %swift.type* (%swift.opaque*, %swift.type*)*
  // CHECK: [[METATYPE:%.*]] = call %swift.type* %typeof(%swift.opaque* [[X]], %swift.type* [[TYPE]])
  // CHECK: ret %swift.type* [[METATYPE]]
  return Builtin.typeof(x)
}

/*
FIXME: CallEmission assertion failure when trying to actually call genericTypeof
struct Foo {}
class Bar {}

func specializedTypeofs(x:Foo, y:Bar) -> (Foo.metatype, Bar.metatype) {
  return (genericTypeof(x), genericTypeof(y))
}
*/
