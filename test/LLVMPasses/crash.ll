;  RUN: %swift-llvm-opt -passes=swift-llvm-arc-optimize %s > /dev/null

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

%swift.refcounted = type { ptr, i64 }
%swift.heapmetadata = type { ptr, ptr }

declare { ptr, i64, ptr } @_Tsop1pFT3lhsNs6String3rhsS__S_(ptr, i64, ptr, ptr, i64, ptr)
declare { ptr, i64, ptr } @_TNs6String24convertFromStringLiteralFT3valp_S_(ptr)
declare void @swift_release(ptr nocapture)


@0 = private unnamed_addr constant [3 x i8] c"So\00"
@1 = private unnamed_addr constant [3 x i8] c"me\00"

; rdar://11558546
define void @release_past_extract() {
entry:
  %0 = call { ptr, i64, ptr } @_TNs6String24convertFromStringLiteralFT3valp_S_(ptr @0)
  %1 = extractvalue { ptr, i64, ptr } %0, 0
  %2 = extractvalue { ptr, i64, ptr } %0, 1
  %3 = extractvalue { ptr, i64, ptr } %0, 2
  %4 = call { ptr, i64, ptr } @_TNs6String24convertFromStringLiteralFT3valp_S_(ptr @1)
  %5 = extractvalue { ptr, i64, ptr } %4, 0
  %6 = extractvalue { ptr, i64, ptr } %4, 1
  %7 = extractvalue { ptr, i64, ptr } %4, 2
  %8 = call { ptr, i64, ptr } @_Tsop1pFT3lhsNs6String3rhsS__S_(ptr %1, i64 %2, ptr %3, ptr %5, i64 %6, ptr %7)
  %9 = extractvalue { ptr, i64, ptr } %8, 0
  %10 = extractvalue { ptr, i64, ptr } %8, 1
  %11 = extractvalue { ptr, i64, ptr } %8, 2
  call void @swift_release(ptr null) nounwind
  call void @swift_release(ptr %11) nounwind
  ret void
}

