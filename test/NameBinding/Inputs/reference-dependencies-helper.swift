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

