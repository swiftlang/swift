public struct GenericStruct<T, U, V> {
  var t: T
  var u: U
  var v: V
  var str: String
}

public struct GenericField<T, U> {
  var field: GenericStruct<T, U, Double>
  var int: Int
}

public struct Box<T> {
  var field: T
}

public struct Box3<T, U, V> {
  var field1: T
  var field2: U
  var field3: V
}

// The protocol conformance puts a symbol into __DATA_CONST which the builder
// can use as the base symbol for references to other data.
public protocol PublicProto {}
extension Box3: PublicProto {}
