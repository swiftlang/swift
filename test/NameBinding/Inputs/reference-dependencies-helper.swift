class ClassFromOtherFile {}

// This Int16 is specifically checked for in the primary file.
typealias AliasFromOtherFile = Int16

func funcFromOtherFile() {}

struct OtherFileOuterType {
  struct InnerType {
    static let sharedConstant = 42
  }
}

struct OtherFileOuterType2 {
  struct InnerType {
  }
}

struct OtherFileSecretTypeWrapper {
  struct SecretType {
    static let constant = 42
  }
}

struct OtherFileIntArray : SequenceType {
  var array: [Int] = []
  func generate() -> Array<Int>.Generator { return array.generate() }
}
func getOtherFileIntArray() -> OtherFileIntArray { return OtherFileIntArray() }

typealias OtherFileAliasForSecret = OtherFileSecretTypeWrapper.SecretType

prefix operator *** {}

typealias OtherFileAliasForFloatLiteralConvertible = FloatLiteralConvertible

enum OtherFileEnum {
  case Value
  case AnotherValue
  case ValueWithPayload(Int)
}

func getOtherFileEnum() -> OtherFileEnum { return .Value }

struct OtherFileEnumWrapper {
  enum Enum {
    case Value
    case AnotherValue
    case ValueWithPayload(Int)
  }
}

protocol OtherFileProto {}
struct OtherFileProtoImplementor : OtherFileProto {}
func otherFileGetImpl() -> OtherFileProtoImplementor {}
func otherFileUse(_: OtherFileProto) {}

protocol OtherFileProto2 {}
struct OtherFileProtoImplementor2 : OtherFileProto2 {}
func otherFileGetImpl2() -> OtherFileProtoImplementor2 {}
func otherFileUseGeneric<T: OtherFileProto2>(_: T) {}

struct OtherFileProtoNonImplementor {}
func otherFileGetNonImpl() -> OtherFileProtoNonImplementor {}

func topLevel1() -> Int { return 2 }
func topLevel2() -> Int { return 2 }
func topLevel3() -> Int { return 2 }
func topLevel4() -> Int { return 2 }
func topLevel5() -> Int { return 2 }
func topLevel6() -> Int { return 2 }
func topLevel7() -> Int { return 2 }
func topLevel8() -> Int { return 2 }
func topLevel9() -> Int { return 2 }

typealias TopLevelTy1 = Int
typealias TopLevelTy2 = Int
typealias TopLevelTy3 = Int
typealias TopLevelTy4 = Int
struct TopLevelStruct {
  typealias ValueType = Int
}
struct TopLevelStruct2 {
  typealias ValueType = Int
}
struct TopLevelStruct3 {
  typealias ValueType = Int
}
struct TopLevelStruct4 {
  typealias ValueType = Int
}

protocol TopLevelProto1 {}
protocol TopLevelProto2 {}
protocol TopLevelProto3 {}

func privateTopLevel1() -> Int { return 2 }
func privateTopLevel2() -> Int { return 2 }
func privateTopLevel3() -> Int { return 2 }
func privateTopLevel4() -> Int { return 2 }
func privateTopLevel5() -> Int { return 2 }
func privateTopLevel6() -> Int { return 2 }
func privateTopLevel7() -> Int { return 2 }
func privateTopLevel8() -> Int { return 2 }
func privateTopLevel9() -> Int { return 2 }

typealias PrivateTopLevelTy1 = Int
typealias PrivateTopLevelTy2 = Int
typealias PrivateTopLevelTy3 = Int
struct PrivateTopLevelStruct {
  typealias ValueType = Int
}
struct PrivateTopLevelStruct2 {
  typealias ValueType = Int
}
struct PrivateTopLevelStruct3 {
  typealias ValueType = Int
}
struct PrivateTopLevelStruct4 {
  typealias ValueType = Int
}

protocol PrivateProto1 {}
protocol PrivateProto2 {}
protocol PrivateProto3 {}

struct OtherFileElementType {}

struct OtherFileTypeToBeExtended {}
