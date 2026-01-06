// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil -module-name=test %s | %FileCheck %s

// REQUIRES: swift_in_compiler

// CHECK-LABEL: sil hidden @$s4test6testitySiAA1CCF :
// CHECK:         ref_element_addr [immutable] %0 : $C, #C.a
// CHECK:       } // end sil function '$s4test6testitySiAA1CCF'
@discardableResult
func testit(_ c: C) -> Int {
  return c.a
}

class C {
  final let a: Int
  final var b: Int

  // CHECK-LABEL: sil hidden @$s4test1CCACycfc :
  // CHECK:         ref_element_addr %0 : $C, #C.a
  // CHECK:         store
  // CHECK:         [[EI:%.*]] = end_init_let_ref %0
  // CHECK:         ref_element_addr [[EI]] : $C, #C.b
  // CHECK:         store
  // CHECK:         [[A:%.*]] = ref_element_addr [immutable] [[EI]] : $C, #C.a
  // CHECK:         [[L:%.*]] = load [[A]]
  // CHECK:         apply {{.*}}([[L]])
  // CHECK:       } // end sil function '$s4test1CCACycfc'
  init() {
    self.a = 1
    self.b = 2
    takeint(self.a)
  }

  // CHECK-LABEL: sil hidden @$s4test1CC1bACSgSb_tcfc :
  // CHECK:       bb1:
  // CHECK:         ref_element_addr %1 : $C, #C.a
  // CHECK:         store
  // CHECK:         [[EI1:%.*]] = end_init_let_ref %1
  // CHECK:         ref_element_addr [[EI1]] : $C, #C.b
  // CHECK:         store
  // CHECK:         apply {{.*}}([[EI1]])
  // CHECK:         br bb3([[EI1]] : $C)
  // CHECK:       bb2:
  // CHECK:         ref_element_addr %1 : $C, #C.a
  // CHECK:         store
  // CHECK:         [[EI2:%.*]] = end_init_let_ref %1
  // CHECK:         ref_element_addr [[EI2]] : $C, #C.b
  // CHECK:         store
  // CHECK:         apply {{.*}}([[EI2]])
  // CHECK:         br bb3([[EI2]] : $C)
  // CHECK:       bb3([[P:%.*]] : $C):
  // CHECK:         [[O:%.*]] = enum $Optional<C>, #Optional.some!enumelt, [[P]]
  // CHECK:         return [[O]]
  // CHECK:       } // end sil function '$s4test1CC1bACSgSb_tcfc'
  init?(b: Bool) {
    if b {
      self.a = 1
      self.b = 2
      testit(self)
    } else {
      self.a = 3
      self.b = 4
      testit(self)
    }
  }

  // CHECK-LABEL: sil hidden @$s4test1CC1cACSb_tcfc :
  // CHECK:         [[A:%.*]] = ref_element_addr %1 : $C, #C.a
  // CHECK:         store {{.*}} to [[A]] : $*Int
  // CHECK:         [[EI:%.*]] = end_init_let_ref %1
  // CHECK:       bb1({{.*}} : $Builtin.Int2):
  // CHECK:         ref_element_addr [[EI]] : $C, #C.b
  // CHECK:         return [[EI]]
  // CHECK:       } // end sil function '$s4test1CC1cACSb_tcfc'
  init(c: Bool) {
    self.a = 1
    repeat {
      self.b = 2
    } while pred()
  }

  // CHECK-LABEL: sil hidden @$s4test1CC1dACSgSb_tcfc :
  // CHECK:         ref_element_addr %1 : $C, #C.a
  // CHECK-NEXT:    store
  // CHECK:         ref_element_addr %1 : $C, #C.a
  // CHECK-NEXT:    begin_access [deinit]
  // CHECK:         end_access
  // CHECK:         end_init_let_ref %1
  // CHECK:       } // end sil function '$s4test1CC1dACSgSb_tcfc'
  init?(d: Bool) {
    self.a = 1
    return nil
  }

  // CHECK-LABEL: sil hidden @$s4test1CC1eACSgSb_tcfc :
  // CHECK:         ref_element_addr %1 : $C, #C.a
  // CHECK:         [[EI:%.*]] = end_init_let_ref %1
  // CHECK:         ref_element_addr [[EI]] : $C, #C.b
  // CHECK:         apply {{.*}}([[EI]])
  // CHECK:       } // end sil function '$s4test1CC1eACSgSb_tcfc'
  init?(e: Bool) {
    self.a = 1
    self.b = 2
    testit(self)
    return nil
  }

  // CHECK-LABEL: sil hidden @$s4test1CC1fACSb_tKcfc :
  // CHECK:         ref_element_addr %1 : $C, #C.a
  // CHECK-NEXT:    store
  // CHECK:         ref_element_addr %1 : $C, #C.a
  // CHECK-NEXT:    begin_access [deinit]
  // CHECK:         end_access
  // CHECK:         end_init_let_ref %1
  // CHECK:       } // end sil function '$s4test1CC1fACSb_tKcfc'
  init(f: Bool) throws {
    self.a = 1
    throw E()
  }

  // CHECK-LABEL: sil hidden @$s4test1CC1gACSb_tKcfc :
  // CHECK:         ref_element_addr %1 : $C, #C.a
  // CHECK:         [[EI:%.*]] = end_init_let_ref %1
  // CHECK:         ref_element_addr [[EI]] : $C, #C.b
  // CHECK:         apply {{.*}}([[EI]])
  // CHECK:       } // end sil function '$s4test1CC1gACSb_tKcfc'
  init(g: Bool) throws {
    self.a = 1
    self.b = 2
    testit(self)
    throw E()
  }

  // CHECK-LABEL: sil hidden @$s4test1CC1hACSgSb_tcfc :
  // CHECK:       bb1:
  // CHECK:         [[EI:%.*]] = end_init_let_ref %1
  // CHECK:         enum {{.*}} #Optional.none
  // CHECK:       bb2:
  // CHECK:         [[EI:%.*]] = end_init_let_ref %1
  // CHECK:         enum {{.*}} #Optional.some!enumelt, [[EI]]
  // CHECK:       } // end sil function '$s4test1CC1hACSgSb_tcfc'
  init?(h: Bool) {
    self.a = 1
    if h {
      return nil
    }
    self.b = 2
  }
}

class X {
  final let c: C

  // CHECK-LABEL: sil hidden @$s4test1XCyAcA1CCcfc :
  // CHECK:         ref_element_addr %1 : $X, #X.c
  // CHECK:         [[EI:%.*]] = end_init_let_ref %1
  // CHECK:         return [[EI]]
  // CHECK:       } // end sil function '$s4test1XCyAcA1CCcfc'
  init(_ c: C) {
    self.c = c
  }

  // CHECK-LABEL: sil hidden @$s4test1XCfd :
  // CHECK:         ref_element_addr %0 : $X, #X.c
  // CHECK:         ref_element_addr %0 : $X, #X.c
  // CHECK:       } // end sil function '$s4test1XCfd'
  deinit {
    testit(c)
  }
}

class D : C {
  final let c: Int

  // CHECK-LABEL: sil hidden @$s4test1DCACycfc :
  // CHECK-NOT:     end_init_let_ref
  // CHECK:         ref_element_addr %{{[0-9]+}} : $D, #D.c
  // CHECK-NOT:     end_init_let_ref
  // CHECK:       } // end sil function '$s4test1DCACycfc'
  override init() {
    self.c = 27
    super.init()
  }
}

func takeint(_ i: Int) {
}

func pred() -> Bool {
  return true
}

struct E : Error {}

