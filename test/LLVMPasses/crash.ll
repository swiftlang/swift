;  RUN: %llvm-opt -swift-arc-optimize -S %s > /dev/null

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

%swift.refcounted = type { %swift.heapmetadata*, i64 }
%swift.heapmetadata = type { i64 (%swift.refcounted*)*, i64 (%swift.refcounted*)* }

declare { i8*, i64, %swift.refcounted* } @_TSsop1pFT3lhsNSs6String3rhsS__S_(i8*, i64, %swift.refcounted*, i8*, i64, %swift.refcounted*)
declare { i8*, i64, %swift.refcounted* } @_TNSs6String24convertFromStringLiteralFT3valp_S_(i8*)
declare void @swift_release(%swift.refcounted* nocapture)


@0 = private unnamed_addr constant [3 x i8] c"So\00"
@1 = private unnamed_addr constant [3 x i8] c"me\00"

; rdar://11558546
define void @release_past_extract() {
entry:
  %0 = call { i8*, i64, %swift.refcounted* } @_TNSs6String24convertFromStringLiteralFT3valp_S_(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @0, i32 0, i32 0))
  %1 = extractvalue { i8*, i64, %swift.refcounted* } %0, 0
  %2 = extractvalue { i8*, i64, %swift.refcounted* } %0, 1
  %3 = extractvalue { i8*, i64, %swift.refcounted* } %0, 2
  %4 = call { i8*, i64, %swift.refcounted* } @_TNSs6String24convertFromStringLiteralFT3valp_S_(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @1, i32 0, i32 0))
  %5 = extractvalue { i8*, i64, %swift.refcounted* } %4, 0
  %6 = extractvalue { i8*, i64, %swift.refcounted* } %4, 1
  %7 = extractvalue { i8*, i64, %swift.refcounted* } %4, 2
  %8 = call { i8*, i64, %swift.refcounted* } @_TSsop1pFT3lhsNSs6String3rhsS__S_(i8* %1, i64 %2, %swift.refcounted* %3, i8* %5, i64 %6, %swift.refcounted* %7)
  %9 = extractvalue { i8*, i64, %swift.refcounted* } %8, 0
  %10 = extractvalue { i8*, i64, %swift.refcounted* } %8, 1
  %11 = extractvalue { i8*, i64, %swift.refcounted* } %8, 2
  call void @swift_release(%swift.refcounted* null) nounwind
  call void @swift_release(%swift.refcounted* %11) nounwind
  ret void
}

