// RUN: %batch-code-completion

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

func testConstructor() {
  MyClass(#^INITIALIZER^#)
// INITIALIZER: Begin completions, 4 items
// INITIALIZER-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ['(']{#init1: Int#}[')'][#MyClass#];
// INITIALIZER-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ['(']{#init2: Int#}[')'][#MyClass#];
// INITIALIZER-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ['(']{#init3: Int#}[')'][#MyClass#];
// INITIALIZER-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ['(']{#init4: Int#}[')'][#MyClass#];
}

func testMethod(obj: MyClass) {
  obj.method(#^METHOD^#)
// METHOD: Begin completions, 4 items
// METHOD-DAG: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]:   ['(']{#method1: Int#}[')'][#Void#];
// METHOD-DAG: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]:   ['(']{#method2: Int#}[')'][#Void#];
// METHOD-DAG: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]:   ['(']{#method3: Int#}[')'][#Void#];
// METHOD-DAG: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]:   ['(']{#method4: Int#}[')'][#Void#];
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
// AVAILABILITY-DAG: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]:   ['(']{#method2: Int#}[')'][#Void#];
// AVAILABILITY-DAG: Decl[InstanceMethod]/Super/Flair[ArgLabels]:         ['(']{#method1: Int#}[')'][#Void#];
}

struct TestStatic {
  static func method(_ self: TestStatic) -> () ->  Void {}
  func method() ->  Void {}
}
func testStaticFunc() {
  TestStatic.method(#^STATIC^#)
// STATIC-DAG: Decl[StaticMethod]/CurrNominal/Flair[ArgLabels]:     ['(']{#(self): TestStatic#}[')'][#() -> Void#];
// STATIC-DAG: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]:   ['(']{#(self): TestStatic#}[')'][#() -> Void#];
}

protocol TestShadowedProtocol {}

extension TestShadowedProtocol {
  func argOverloaded(arg: String) {}
  func argOverloaded(arg: Int) {}

  func returnTypeOverloaded() -> String {}
  func returnTypeOverloaded() -> Int {}
}

struct TestShadowedStruct: TestShadowedProtocol {
  func argOverloaded(arg: String) {}

  func returnTypeOverloaded() -> String {}

  func test() {
    self.argOverloaded(#^ARG_OVERLOADED^#)
    // ARG_OVERLOADED: Begin completions, 2 items
    // ARG_OVERLOADED-DAG: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]: ['(']{#arg: String#}[')'][#Void#]; name=arg:
    // ARG_OVERLOADED-DAG: Decl[InstanceMethod]/Super/Flair[ArgLabels]: ['(']{#arg: Int#}[')'][#Void#]; name=arg:

    self.returnTypeOverloaded(#^RETURN_OVERLOADED^#)
    // RETURN_OVERLOADED: Begin completions, 2 items
    // RETURN_OVERLOADED-DAG: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]: ['('][')'][#String#]; name=
    // RETURN_OVERLOADED-DAG: Decl[InstanceMethod]/Super/Flair[ArgLabels]: ['('][')'][#Int#]; name=
  }
}
