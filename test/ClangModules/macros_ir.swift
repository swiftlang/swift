// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -emit-ir -o - %s | FileCheck %s

import macros

// CHECK-DAG: [[OBJC_STRING:@[0-9]+]] = private unnamed_addr constant [11 x i16] [i16 85, i16 110, i16 105, i16 99, i16 111, i16 100, i16 101, i16 33, i16 32, i16 10024, i16 0]
// CHECK-DAG: [[UTF8_STRING:@[0-9]+]] = private unnamed_addr constant [11 x i8] c"Swift \F0\9F\8F\83\00"
// CHECK-DAG: [[VERSION_STRING:@[0-9]+]] = private unnamed_addr constant [10 x i8] c"Swift 1.0\00"


// FIXME: Decide the type macros should map to.

// CHECK-LABEL: define double @_TF9macros_ir11circle_areaFT6radiusSd_Sd
func circle_area(#radius: CDouble) -> CDouble {
  // CHECK: call double @_TFSCg4M_PISd
  return M_PI * radius * radius
}

// CHECK-LABEL: define linkonce_odr hidden double @_TFSCg4M_PISd
// CHECK: ret double 0x400921FB54442D11


func get_eof() -> CInt {
  return EOF
}

// CHECK-LABEL: define linkonce_odr hidden i32 @_TFSCg3EOFVSs5Int32
// CHECK-NOT: ret i32
// CHECK: store i32 1, i32* {{.*}}, align 4
// CHECK: call void @_TFSsop1sUSs13_SignedNumber_{{.*}}_FQ_Q_
// CHECK: ret i32


func testCStrings() -> Bool {
    return !VERSION_STRING.isNull() && !UTF8_STRING.isNull()
}

// CHECK-LABEL: define linkonce_odr hidden i8* @_TFSCg14VERSION_STRINGVSs7CString
// CHECK-NOT: ret
// CHECK: [[RESULT:%.+]] = call i8* @_TFVSs7CString32_convertFromBuiltinStringLiteralfMS_FTBp8byteSizeBw7isASCIIBi1__S_(i8* getelementptr inbounds ([10 x i8]* [[VERSION_STRING]], i64 0, i64 0), i64 9, i1 true)
// CHECK: ret i8* [[RESULT]]

// CHECK-LABEL: define linkonce_odr hidden i8* @_TFSCg11UTF8_STRINGVSs7CString
// CHECK-NOT: ret
// CHECK: [[RESULT:%.+]] = call i8* @_TFVSs7CString32_convertFromBuiltinStringLiteralfMS_FTBp8byteSizeBw7isASCIIBi1__S_(i8* getelementptr inbounds ([11 x i8]* [[UTF8_STRING]], i64 0, i64 0), i64 10, i1 false)
// CHECK: ret i8* [[RESULT]]


func testObjCString() -> Int {
  return OBJC_STRING._encodedLength(UTF8.self)
}

// CHECK-LABEL: define linkonce_odr hidden { i8*, i64, i64 } @_TFSCg11OBJC_STRINGSS
// CHECK-NOT: ret
// CHECK: = call { i8*, i64, i64 } @_TFSS37_convertFromBuiltinUTF16StringLiteralfMSSFTBp17numberOfCodeUnitsBw_SS(i8* bitcast ([11 x i16]* [[OBJC_STRING]] to i8*), i64 10)

// CHECK-LABEL: define linkonce_odr hidden i64 @_TFSCg11BIT_SHIFT_3VSs5Int64
// CHECK-NOT: }
// CHECK: ret i64 24
// CHECK-NEXT: }

func testBitShift() -> CLongLong {
  return BIT_SHIFT_3
}
