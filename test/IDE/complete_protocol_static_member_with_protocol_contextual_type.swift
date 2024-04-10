// RUN: %batch-code-completion

protocol MyProto {}
protocol MyOtherProto: MyProto {}
struct MyStruct : MyProto {}
extension MyProto where Self == MyStruct {
  static var constrainedOnMyStruct: MyStruct { fatalError() }
}
extension MyProto where Self: MyOtherProto {
  static var constrainedOnMyInheritanceOfOtherProto: MyOtherProto { fatalError() }
}
extension MyProto where Self == MyOtherProto {
  static var constrainedOnMyEqualityOfOtherProto: MyOtherProto { fatalError() }
}


func testOnMyProto() {
  let _: MyProto = .#^ON_MY_PROTO^#
// ON_MY_PROTO: Begin completions, 1 items
// ON_MY_PROTO-DAG: Decl[StaticVar]/CurrNominal/TypeRelation[Convertible]: constrainedOnMyStruct[#MyStruct#];
}

func testOnMyOtherProto() {
  // constrainedOnMyStruct is not valid here
  let _: MyOtherProto = .#^ON_MY_OTHER_PROTO^#
// ON_MY_OTHER_PROTO-NOT: Begin completions
}

func testOpaqueMyProto() -> some MyProto {
   return .#^IN_OPAQUE_PROTOCOL_POS?check=ON_MY_PROTO^#
}
