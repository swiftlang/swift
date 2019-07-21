protocol Proto {
  var prop: Int { get set }
}
extension Proto {
  func method() {}
}

var globalExistential: Proto {
  fatalError()
}

protocol ClassProtoBase: AnyObject {
  var baseProp: Int { get set }
}
protocol ClassProto: ClassProtoBase {
  var prop: Int { get set }
}
func getClassExistential() -> ClassProto? {
  fatalError()
}
