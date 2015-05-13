// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -parse -verify -primary-file %t/main.swift %S/Inputs/enum_errortype_other.swift

enum ClericalErrorDomain: ErrorType {
  case MisplacedDocument(name: String)
  case AccidentallyErasedTape(fromMinute: Double, toMinute: Double)
}

let error
  = ClericalErrorDomain.AccidentallyErasedTape(fromMinute: 5, toMinute: 23.5)

let domain: String = error._domain
let code: Int = error._code

struct UseEnumBeforeDeclaration {
  let errorDomain: String = EnumToUseBeforeDeclaration.A._domain
  let errorCode: Int = EnumToUseBeforeDeclaration.A._code
}
enum EnumToUseBeforeDeclaration: ErrorType {
  case A
}

let domainFromOtherFile: String = FromOtherFile.A._domain
let codeFromOtherFile: Int = AlsoFromOtherFile.A._code

enum NotAnError { case A }

let notAnErrorDomain: String = NotAnError.A._domain // expected-error{{'NotAnError' does not have a member named '_domain'}}
let notAnErrorCode: Int = NotAnError.A._code // expected-error{{'NotAnError' does not have a member named '_code'}}

enum EmptyErrorDomain: ErrorType {}
