// RUN: %target-typecheck-verify-swift

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
