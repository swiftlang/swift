; RUN: %swift-llvm-opt -passes='swift-merge-functions' -swiftmergefunc-threshold=2 %s | %FileCheck %s

@g1 = external global i1
@g2 = external global i1
@g3 = external global i1

declare { ptr, i1 } @llvm.type.checked.load(ptr, i32, metadata)

define i1 @merge_candidate_a(ptr %ptr, i32 %offset) {
    %1 = call { ptr, i1 } @llvm.type.checked.load(ptr %ptr, i32 %offset, metadata !"common_metadata")
    %2 = extractvalue { ptr, i1 } %1, 1
    %3 = load i1, i1* @g1
    %4 = and i1 %2, %3
    ret i1 %4
}

; The function using common metadata should call into the merged function

define i1 @merge_candidate_b(ptr %ptr, i32 %offset) {
    %1 = call { ptr, i1 } @llvm.type.checked.load(ptr %ptr, i32 %offset, metadata !"common_metadata")
    %2 = extractvalue { ptr, i1 } %1, 1
    %3 = load i1, i1* @g2
    %4 = and i1 %2, %3
    ret i1 %4
}
; CHECK-LABEL: @merge_candidate_b
; CHECK:       call i1 @merge_candidate_aTm
; CHECK:       ret

; The function using different metadata should not

define i1 @merge_candidate_c(ptr %ptr, i32 %offset) {
    %1 = call { ptr, i1 } @llvm.type.checked.load(ptr %ptr, i32 %offset, metadata !"different_metadata")
    %2 = extractvalue { ptr, i1 } %1, 1
    %3 = load i1, i1* @g3
    %4 = and i1 %2, %3
    ret i1 %4
}
; CHECK-LABEL: @merge_candidate_c
; CHECK-NOT:   call i1 @merge_candidate_aTm
; CHECK:       @llvm.type.checked.load(ptr %ptr, i32 %offset, metadata !"different_metadata")
; CHECK-NOT:   call i1 @merge_candidate_aTm
; CHECK:       ret
