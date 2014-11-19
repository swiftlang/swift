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
