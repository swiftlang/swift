func sink(_ a: (Int) -> Void) {}
    
func testSingleStatementClosure() {
  sink { items in
    // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):9 %s -- %s | %FileCheck %s --check-prefix=SINGLE-STMT-CLOSURE
    var items = items
  }
  // SINGLE-STMT-CLOSURE: s:13discriminator26testSingleStatementClosureyyFySiXEfU_5itemsL0_Sivp
}


func testMultiStatementClosure() {
  sink { items in
    // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):9 %s -- %s | %FileCheck %s --check-prefix=MULTI-STMT-CLOSURE
    var items = items
    print("xxx")
  }
  // MULTI-STMT-CLOSURE: s:13discriminator25testMultiStatementClosureyyFySiXEfU_5itemsL0_Sivp
}

func testNestedClosures() {
  func dataTask(completionHandler: (Int?) -> Void) {}
  func async2(execute work: () -> Void) {}

  dataTask { (error) in
    do {
      // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):17 %s -- %s | %FileCheck %s --check-prefix=NESTED_CLOSURE
    } catch let error {
      async2 {
        print("")
      }
    }
  }
  // NESTED_CLOSURE: s:13discriminator18testNestedClosuresyyFySiSgXEfU_5errorL1_s5Error_pvp
}

func testReuseAST(bytes: Int) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line + 2):7 %s -- %s == \
  // RUN:   -req=cursor -pos=%(line + 2):7 %s -- %s | %FileCheck %s --check-prefix=REUSE_AST
  let size = 3
  var bytes = 6
  // REUSE_AST: source.lang.swift.decl.var.local (40:7-40:11)
  // REUSE_AST: s:13discriminator12testReuseAST5bytesySi_tF4sizeL_Sivp
  // REUSE_AST: source.lang.swift.decl.var.local (41:7-41:12)
  // REUSE_AST: s:13discriminator12testReuseAST5bytesySi_tFACL0_Sivp
}
