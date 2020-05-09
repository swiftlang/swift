// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE_1 -source-filename=%s
// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE_2 -source-filename=%s

struct MyStruct<T> {
}

protocol MyProto {
  associatedtype Assoc
  func foo(x: Assoc) -> Assoc
}

#if false
extension MyStruct {
  #^COMPLETE_1^#
}

extension MyStruct: MyProto {
  #^COMPLETE_2^#
}
#endif
