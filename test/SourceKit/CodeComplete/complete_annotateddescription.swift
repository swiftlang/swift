struct MyStruct {
  func labelNameParamName(label param: (inout Int) throws -> MyStruct) rethrows {}
  func labelName(label: (@autoclosure () -> Int) -> Int) {}
  func sameName(label label: Int) {}
  func paramName(_ param: Int) {}
  subscript(param: Int) -> Int { 1 }
  subscript(label param: Int) -> Int { 1 }
}
func test(value: MyStruct) {
  value 
}
func test() {
  MyStruct.
}

// RUN: %sourcekitd-test \
// RUN: -req=complete -pos=10:8 -req-opts=annotateddescription=1 %s -- %s == \
// RUN: -req=complete -pos=13:12 -req-opts=annotateddescription=1 %s -- %s > %t.result
// RUN: %diff -u %s.result %t.result
