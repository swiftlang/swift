// RUN: %target-typecheck-verify-swift

let a: Int? = 1
guard let b = a else {
}

func foo() {} // to interrupt the TopLevelCodeDecl

let c = b
