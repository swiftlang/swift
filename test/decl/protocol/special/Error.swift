// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -typecheck -verify -primary-file %t/main.swift %S/Inputs/Error_other.swift

enum ClericalErrorDomain: Error {
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
enum EnumToUseBeforeDeclaration: Error {
  case A
}

let domainFromOtherFile: String = FromOtherFile.A._domain
let codeFromOtherFile: Int = AlsoFromOtherFile.A._code

enum NotAnError { case A }

let notAnErrorDomain: String = NotAnError.A._domain // expected-error{{value of type 'NotAnError' has no member '_domain'}}
let notAnErrorCode: Int = NotAnError.A._code // expected-error{{value of type 'NotAnError' has no member '_code'}}

enum EmptyErrorDomain: Error {}

struct ErrorStruct : Error {
}

class ErrorClass : Error {
}

struct ErrorStruct2 { }

extension ErrorStruct2 : Error { }

class ErrorClass2 { }

extension ErrorClass2 : Error { }
