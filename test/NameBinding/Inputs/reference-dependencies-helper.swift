class ClassFromOtherFile {}

// This Int16 is specifically checked for in the primary file.
typealias AliasFromOtherFile = Int16

func funcFromOtherFile() {}

struct OtherFileOuterType {
  struct InnerType {
    static let sharedConstant = 42
  }
}

struct OtherFileSecretTypeWrapper {
  struct SecretType {
    static let constant = 42
  }
}

typealias OtherFileAliasForSecret = OtherFileSecretTypeWrapper.SecretType

prefix operator *** {}

typealias OtherFileAliasForFloatLiteralConvertible = FloatLiteralConvertible


protocol OtherFileProto {}
struct OtherFileProtoImplementor : OtherFileProto {}
func otherFileGetImpl() -> OtherFileProtoImplementor {}
func otherFileUse(_: OtherFileProto) {}

protocol OtherFileProto2 {}
struct OtherFileProtoImplementor2 : OtherFileProto2 {}
func otherFileGetImpl2() -> OtherFileProtoImplementor2 {}
func otherFileUseGeneric<T: OtherFileProto2>(_: T) {}
