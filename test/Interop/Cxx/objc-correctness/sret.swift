// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-emit-ir -I %t/Inputs -cxx-interoperability-mode=default %t/test.swift -target arm64-apple-macos12 | %FileCheck %s

// REQUIRES: objc_interop

//--- Inputs/header.h

class S {
public:
  int a;
  ~S();
  S getS(int) const;
  static S getSStatic(int);
};

S getS(int);

@interface C
+(S)getS:(int)a;
-(S)getS:(int)a;
@end

//--- Inputs/module.modulemap

module SRet {
  header "header.h"
  requires cplusplus
  export *
}

//--- test.swift

import SRet

func test(c : C, s : S) {
  let _ : S = c.getS(1)
  let _ : S = C.getS(1)
  let _ : S = S()
  let _ : S = getS(1)
  let _ : S = s.getS(1)
  let _ : S = S.getSStatic(1)
}

// CHECK: %[[TSO1SV:.*]] = type <{ %[[TS5INT32V:.*]] }>
// CHECK: %[[TS5INT32V]] = type <{ i32 }>

// CHECK: define hidden swiftcc void @"$s4testAA1c1sySo1CC_So1SVtF"(ptr %[[V0:.*]], ptr {{.*}}%[[V1:.*]])
// CHECK: %[[V2:.*]] = alloca %[[TSO1SV]], align 4
// CHECK: %[[V3:.*]] = alloca %[[TSO1SV]], align 4
// CHECK: %[[V4:.*]] = alloca %[[TSO1SV]], align 4
// CHECK: %[[V5:.*]] = alloca %[[TSO1SV]], align 4
// CHECK: %[[V6:.*]] = alloca %[[TSO1SV]], align 4
// CHECK: %[[V7:.*]] = alloca %[[TSO1SV]], align 4
// CHECK: call void @llvm.lifetime.start.p0(i64 4, ptr %[[V2]])
// CHECK: %[[V8:.*]] = load ptr, ptr @"\01L_selector(getS:)", align 8
// CHECK: invoke void @objc_msgSend(ptr noalias sret(%[[TSO1SV]]) %[[V2]], ptr %[[V0]], ptr %[[V8]], i32 1)

// CHECK: %[[V10:.*]] = load ptr, ptr @"OBJC_CLASS_REF_$_C", align 8
// CHECK: %[[V11:.*]] = call ptr @objc_opt_self(ptr %[[V10]])
// CHECK: %[[V12:.*]] = load ptr, ptr @"\01L_selector(getS:)", align 8
// CHECK: invoke void @objc_msgSend(ptr noalias sret(%[[TSO1SV]]) %[[V3]], ptr %[[V11]], ptr %[[V12]], i32 1)

// CHECK: %[[V14:.*]] = call ptr @_ZN1SC1Ev(ptr %[[V4]])
// CHECK: invoke void @_Z4getSi(ptr noalias sret(%[[TSO1SV]]) %[[V5]], i32 1)

// CHECK: invoke void @_ZNK1S4getSEi(ptr noalias sret(%[[TSO1SV]]) %[[V6]], ptr %[[V1]], i32 1)

// CHECK: invoke void @_ZN1S10getSStaticEi(ptr noalias sret(%[[TSO1SV]]) %[[V7]], i32 1)
