; RUN: %swift -target x86_64-apple-darwin10 -emit-object %s

; REQUIRES: CPU=x86_64

; Make sure we update the datalayout with the current clang's. clang knows
; better about the ABI intricacies.
; This test case would crash if we did not do this because of i64:128 in the
; string below.

target datalayout = "e-m:o-i64:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.13.0"

define swiftcc i64 @"$s11TestBitcode3add1x1yS2i_SitF"(i64, i64) #0 {
entry:
  %2 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %0, i64 %1)
  %3 = extractvalue { i64, i1 } %2, 0
  %4 = extractvalue { i64, i1 } %2, 1
  br i1 %4, label %6, label %5

  ret i64 %3

  call void @llvm.trap()
  unreachable
}

declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

declare void @llvm.trap() #2

attributes #0 = { "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" }
attributes #1 = { nounwind readnone speculatable }
attributes #2 = { noreturn nounwind }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6, !7}
!llvm.linker.options = !{!8, !9, !10}

!0 = !{i32 1, !"Objective-C Version", i32 2}
!1 = !{i32 1, !"Objective-C Image Info Version", i32 0}
!2 = !{i32 1, !"Objective-C Image Info Section", !"__DATA,__objc_imageinfo,regular,no_dead_strip"}
!3 = !{i32 4, !"Objective-C Garbage Collection", i32 1536}
!4 = !{i32 1, !"Objective-C Class Properties", i32 64}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{i32 7, !"PIC Level", i32 2}
!7 = !{i32 1, !"Swift Version", i32 6}
!8 = !{!"-lswiftSwiftOnoneSupport"}
!9 = !{!"-lswiftCore"}
!10 = !{!"-lobjc"}
