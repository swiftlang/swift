// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -parse -verify -primary-file %t/main.swift %S/Inputs/enum_errortype_other.swift

enum ClericalErrorDomain: _ErrorType {
  case MisplacedDocument(name: String)
  case AccidentallyErasedTape(fromMinute: Double, toMinute: Double)
}

let error
  = ClericalErrorDomain.AccidentallyErasedTape(fromMinute: 5, toMinute: 23.5)

let domain: String = error.domain
let code: Int = error.code

struct UseEnumBeforeDeclaration {
  let errorDomain: String = EnumToUseBeforeDeclaration.A.domain
  let errorCode: Int = EnumToUseBeforeDeclaration.A.code
}
enum EnumToUseBeforeDeclaration: _ErrorType {
  case A
}

let domainFromOtherFile: String = FromOtherFile.A.domain
let codeFromOtherFile: Int = AlsoFromOtherFile.A.code

enum NotAnError { case A }

let notAnErrorDomain: String = NotAnError.A.domain // expected-error{{'NotAnError' does not have a member named 'domain'}}
let notAnErrorCode: Int = NotAnError.A.code // expected-error{{'NotAnError' does not have a member named 'code'}}

enum EmptyErrorDomain: _ErrorType {}
