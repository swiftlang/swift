struct MyStruct {
  var identifier: String
}

func takeClosure(_ x: () -> Void) {}

func test() {
  takeClosure {
    let foo = MyStruct()
    // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):5 %s -- %s
    foo.identifier = "\(item.category)#\(item.name)"
  }
}
