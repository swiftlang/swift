// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=INITIALIZER | %FileCheck %s --check-prefix=INITIALIZER
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=METHOD | %FileCheck %s --check-prefix=METHOD
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=AVAILABILITY | %FileCheck %s --check-prefix=AVAILABILITY
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STATIC | %FileCheck %s --check-prefix=STATIC

protocol MyProtocol {
  init(init1: Int)
  init(init2: Int)

  func method(method1: Int)
  func method(method2: Int)
}

extension MyProtocol {
  init(init2: Int) { self.init(init1: init2) }
  init(init3: Int) { self.init(init1: init3) }

  func method(method2: Int) {}
  func method(method3: Int) {}
}

class Base {
  init(init4: Int) { }
  func method(method4: Int) {}
}

class MyClass: Base, MyProtocol {

  required init(init1: Int) { super.init(init4: init1) }
  required init(init2: Int) { super.init(init4: init1) }
  init(init3: Int) { super.init(init4: init1) }
  override init(init4: Int) { super.init(init4: init1) }

  func method(method1: Int)
  func method(method2: Int) {}
  func method(method3: Int) {}
  override func method(method4: Int) {}
}

func testConstructer() {
  MyClass(#^INITIALIZER^#)
// INITIALIZER: Begin completions, 4 items
// INITIALIZER-DAG: Decl[Constructor]/CurrNominal:      ['(']{#init1: Int#}[')'][#MyClass#];
// INITIALIZER-DAG: Decl[Constructor]/CurrNominal:      ['(']{#init2: Int#}[')'][#MyClass#];
// INITIALIZER-DAG: Decl[Constructor]/CurrNominal:      ['(']{#init3: Int#}[')'][#MyClass#];
// INITIALIZER-DAG: Decl[Constructor]/CurrNominal:      ['(']{#init4: Int#}[')'][#MyClass#];
// INITIALIZER: End completions
}

func testMethod(obj: MyClass) {
  obj.method(#^METHOD^#)
// METHOD: Begin completions, 4 items
// METHOD-DAG: Decl[InstanceMethod]/CurrNominal:   ['(']{#method1: Int#}[')'][#Void#];
// METHOD-DAG: Decl[InstanceMethod]/CurrNominal:   ['(']{#method2: Int#}[')'][#Void#];
// METHOD-DAG: Decl[InstanceMethod]/CurrNominal:   ['(']{#method3: Int#}[')'][#Void#];
// METHOD-DAG: Decl[InstanceMethod]/CurrNominal:   ['(']{#method4: Int#}[')'][#Void#];
// METHOD: End completions
}

protocol HasUnavailable {}
extension HasUnavailable {
  func method(method1: Int) {}

  @available(*, unavailable)
  func method(method2: Int) {}
}
struct MyStruct: HasUnavailable {
  @available(*, unavailable)
  func method(method1: Int) {}

  func method(method2: Int) {}
}
func testUnavailable(val: MyStruct) {
  val.method(#^AVAILABILITY^#)
// AVAILABILITY: Begin completions, 2 items
// AVAILABILITY-DAG: Decl[InstanceMethod]/CurrNominal:   ['(']{#method2: Int#}[')'][#Void#];
// AVAILABILITY-DAG: Decl[InstanceMethod]/Super:         ['(']{#method1: Int#}[')'][#Void#];
// AVAILABILITY: End completions
}

struct TestStatic {
  static func method(_ self: TestStatic) -> () ->  Void {}
  func method() ->  Void {}
}
func testStaticFunc() {
  TestStatic.method(#^STATIC^#)
// STATIC: Begin completions
// STATIC-DAG: Decl[StaticMethod]/CurrNominal:     ['(']{#(self): TestStatic#}[')'][#() -> Void#];
// STATIC-DAG: Decl[InstanceMethod]/CurrNominal:   ['(']{#(self): TestStatic#}[')'][#() -> Void#];
// STATIC: End completions
}
