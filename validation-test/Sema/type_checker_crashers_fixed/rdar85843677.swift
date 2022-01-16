// RUN: %target-typecheck-verify-swift -swift-version 5 -experimental-multi-statement-closures

func callClosure(closure: () -> ()) {  
  closure()
}

callClosure {
  struct MyStruct {
    let s = "hello"
    lazy var sn: String = { s }()
  }
  
  let s = MyStruct()
  print(s)
}
