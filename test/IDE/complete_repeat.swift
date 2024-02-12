// RUN: %target-swift-ide-test -code-completion -code-completion-token=REPEAT_1 -source-filename=%s | %FileCheck %s -check-prefix=REPEAT_1
// RUN: %target-swift-ide-test -code-completion -code-completion-token=REPEAT_2 -source-filename=%s | %FileCheck %s -check-prefix=REPEAT_2
// RUN: %target-swift-ide-test -code-completion -code-completion-token=REPEAT_3 -source-filename=%s | %FileCheck %s -check-prefix=REPEAT_3
// RUN: %target-swift-ide-test -code-completion -code-completion-token=REPEAT_4 -source-filename=%s | %FileCheck %s -check-prefix=REPEAT_4
// RUN: %target-swift-ide-test -code-completion -code-completion-token=REPEAT_5 -source-filename=%s | %FileCheck %s -check-prefix=REPEAT_5
// RUN: %target-swift-ide-test -code-completion -code-completion-token=REPEAT_COND_1 -source-filename=%s | %FileCheck %s -check-prefix=REPEAT_COND_1

repeat {
  let local1 = 1
  #^REPEAT_1^#
} while true
// REPEAT_1-NOT: LocalVar
// REPEAT_1: Decl[LocalVar]/Local:               local1[#Int#];
// REPEAT_1-NOT: LocalVar

repeat {
  let local1 = 1
  #^REPEAT_2^#
}
// REPEAT_2-NOT: LocalVar
// REPEAT_2: Decl[LocalVar]/Local:               local1[#Int#];
// REPEAT_2-NOT: LocalVar

repeat {
  let local1 = 1
  repeat {
    let local2 = 1
    #^REPEAT_3^#
  } while true
} while true
// REPEAT_3-NOT: LocalVar
// REPEAT_3: Decl[LocalVar]/Local:               local2[#Int#];
// REPEAT_3-NOT: LocalVar
// REPEAT_3: Decl[LocalVar]/Local:               local1[#Int#];
// REPEAT_3-NOT: LocalVar

func enclosingFunc1() {
  let local0 = 1
  repeat {
    let local1 = 1
    repeat {
      let local2 = 1
      #^REPEAT_4^#
    } while true
  } while true
}
// REPEAT_4-NOT: LocalVar
// REPEAT_4: Decl[LocalVar]/Local:               local2[#Int#];
// REPEAT_4-NOT: LocalVar
// REPEAT_4: Decl[LocalVar]/Local:               local1[#Int#];
// REPEAT_4-NOT: LocalVar
// REPEAT_4: Decl[LocalVar]/Local:               local0[#Int#];
// REPEAT_4-NOT: LocalVar

do {
  repeat {
    let local1 = 1
    #^REPEAT_5^#
  } while
}
// REPEAT_5-NOT: LocalVar
// REPEAT_5: Decl[LocalVar]/Local:               local1[#Int#];
// REPEAT_5-NOT: LocalVar

do {
  repeat {
    let local1 = 1
    repeat {
      let local2 = 1
    } while #^REPEAT_COND_1^#
  }
}
// REPEAT_COND_1-NOT: LocalVar
// REPEAT_COND_1: Decl[LocalVar]/Local:               local1[#Int#];
// REPEAT_COND_1-NOT: LocalVar
