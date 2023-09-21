// RUN: %batch-code-completion

struct MyStruct {
    init() {}
    var value: Int
}

// MEMBER_MyStruct: Begin completions, 2 items
// MEMBER_MyStruct-DAG: Keyword[self]/CurrNominal:          self[#MyStruct#];
// MEMBER_MyStruct-DAG: Decl[InstanceVar]/CurrNominal:      value[#Int#];

#if true
let toplevelActive = MyStruct()
_ = toplevelActive.#^MEMBER_TOPLEVEL_ACTIVE?check=MEMBER_MyStruct^#
#else
let toplevelInactive = MyStruct()
_ = toplevelInactive.#^MEMBER_TOPLEVEL_INACTIVE?check=MEMBER_MyStruct^#
#endif

func foo() {
#if true
  let infuncActive = MyStruct()
  _ = infuncActive.#^MEMBER_INFUNC_ACTIVE?check=MEMBER_MyStruct^#
#else
  let infuncInactive = MyStruct()
  _ = infuncInactive.#^MEMBER_INFUNC_INACTIVE?check=MEMBER_MyStruct^#
#endif
}

protocol TestP {
  func foo()
  func bar()
}
struct TestStruct: TestP {
#if true
  func foo() {}
  func #^OVERRIDE_ACTIVE^#
// OVERRIDE_ACTIVE: Begin completions, 1 items
// OVERRIDE_ACTIVE-DAG: Decl[InstanceMethod]/Super:         bar() {|};
#else
  func bar() {}
  func #^OVERRIDE_INACTIVE^#
// OVERRIDE_INACTIVE: Begin completions, 1 items
// OVERRIDE_INACTIVE-DAG: Decl[InstanceMethod]/Super:         foo() {|};
#endif
}

struct TestStruct2 {
#if true
  func activeFunc() {}
  func test() {
    self.#^SELF_ACTIVE^#
  }
// SELF_ACTIVE: Begin completions, 3 items
// SELF_ACTIVE-DAG: Keyword[self]/CurrNominal:          self[#TestStruct2#];
// SELF_ACTIVE-DAG: Decl[InstanceMethod]/CurrNominal:   activeFunc()[#Void#];
// SELF_ACTIVE-DAG: Decl[InstanceMethod]/CurrNominal:   test()[#Void#];
#else
  func inactiveFunc() {}
  func test() {
    self.#^SELF_INACTIVE^#
  }
// SELF_INACTIVE: Begin completions, 3 items
// SELF_INACTIVE-DAG: Keyword[self]/CurrNominal:          self[#TestStruct2#];
// SELF_INACTIVE-DAG: Decl[InstanceMethod]/CurrNominal:   inactiveFunc()[#Void#];
// SELF_INACTIVE-DAG: Decl[InstanceMethod]/CurrNominal:   test()[#Void#];
#endif
}

