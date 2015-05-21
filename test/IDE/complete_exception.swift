// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CATCH1 | FileCheck %s -check-prefix=CATCH1
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=THROW1 > %t.throw1
// RUN: FileCheck %s -check-prefix=THROW1 < %t.throw1
// RUN: FileCheck %s -check-prefix=THROW1-LOCAL < %t.throw1
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CATCH2 | FileCheck %s -check-prefix=CATCH2
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=THROW2 | FileCheck %s -check-prefix=THROW2
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CATCH3 | FileCheck %s -check-prefix=CATCH3
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_CATCH1 | FileCheck %s -check-prefix=CATCH1
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_THROW1 | FileCheck %s -check-prefix=THROW1

// FIXME: <rdar://problem/21001526> No dot code completion results in switch case or catch stmt at top-level
// RUNdisabled: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_CATCH2 | FileCheck %s -check-prefix=CATCH2
// RUNdisabled: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_THROW2 | FileCheck %s -check-prefix=THROW2

// REQUIRES: objc_interop

import Foundation // importer SDK

protocol ErrorPro1 : ErrorType {}
class Error1 : ErrorType {}
class Error2 : ErrorType {}
class Error3 {}
extension Error3 : ErrorType{}
enum Error4 : ErrorType {
  case E1
  case E2(Int)
}
class NoneError1 {}

func getError1() -> Error1 { return Error1() }
func getNSError() -> NSError { return NSError(domain: "", code: 1, userInfo: [:]) }

func test001() {
  do {} catch #^CATCH1^#

// CATCH1:      Begin completions
// CATCH1-DAG:  Decl[Enum]/CurrModule:              Error4[#Error4#]; name=Error4{{$}}
// CATCH1-DAG:  Decl[Class]/CurrModule:             Error3[#Error3#]; name=Error3{{$}}
// CATCH1-DAG:  Decl[Class]/CurrModule:             Error2[#Error2#]; name=Error2{{$}}
// CATCH1-DAG:  Decl[Class]/CurrModule:             Error1[#Error1#]; name=Error1{{$}}
// CATCH1-DAG:  Keyword/None:                       let{{; name=.+$}}
// CATCH1-DAG:  Decl[Class]/CurrModule:             NoneError1[#NoneError1#]; name=NoneError1{{$}}
// CATCH1-DAG:  Decl[Class]/OtherModule[Foundation]: NSError[#NSError#]{{; name=.+$}}
}

func test002() {
  let text = "NonError"
  let e1 = Error1()
  let e2 = Error2()
  throw #^THROW1^#

// THROW1:      Begin completions
// THROW1-DAG:  Decl[Enum]/CurrModule:              Error4[#Error4#]; name=Error4{{$}}
// THROW1-DAG:  Decl[Class]/CurrModule:             Error3[#Error3#]; name=Error3{{$}}
// THROW1-DAG:  Decl[Class]/CurrModule:             Error2[#Error2#]; name=Error2{{$}}
// THROW1-DAG:  Decl[Class]/CurrModule:             Error1[#Error1#]; name=Error1{{$}}
// THROW1-DAG:  Decl[Protocol]/CurrModule:          ErrorPro1[#ErrorPro1#]; name=ErrorPro1{{$}}
// THROW1-DAG:  Decl[FreeFunction]/CurrModule:      getError1()[#Error1#]{{; name=.+$}}
// THROW1-DAG:  Decl[FreeFunction]/CurrModule:      getNSError()[#NSError#]{{; name=.+$}}

// If we could prove that there is no way to get to an ErrorType value by
// starting from these, we could remove them.  But that may be infeasible in
// the presence of overloaded operators.
// THROW1-DAG: Decl[Class]/CurrModule:             NoneError1[#NoneError1#]; name=NoneError1{{$}}
// THROW1-LOCAL: Decl[LocalVar]/Local:               text[#String#]; name=text{{$}}
// THROW1-LOCAL: Decl[LocalVar]/Local:               e1[#Error1#]; name=e1{{$}}
// THROW1-LOCAL: Decl[LocalVar]/Local:               e2[#Error2#]; name=e2{{$}}
}

func test003() {
  do {} catch Error4.#^CATCH2^#
// CATCH2: Begin completions
// CATCH2: Decl[EnumElement]/CurrNominal: E1[#Error4#]{{; name=.+$}}
// CATCH2: Decl[EnumElement]/CurrNominal: E2({#Int#})[#(Int) -> Error4#]{{; name=.+$}}
// CATCH2: End completions
}

func test004() {
  throw Error4.#^THROW2^#
// THROW2: Begin completions
// THROW2: Decl[EnumElement]/CurrNominal: E1[#Error4#]{{; name=.+$}}
// THROW2: Decl[EnumElement]/CurrNominal: E2({#Int#})[#(Int) -> Error4#]{{; name=.+$}}
// THROW2: End completions
}

func test005() {
  do {} catch Error4.E2#^CATCH3^#
// CATCH3: Begin completions
// CATCH3: Pattern/ExprSpecific:               ({#Int#})[#Error4#]{{; name=.+$}}
// CATCH3: End completions
}

// Top-level:
do {} catch #^TOP_LEVEL_CATCH1^# {}
throw #^TOP_LEVEL_THROW1^#
do {} catch Error4.#^TOP_LEVEL_CATCH2^# {}
throw Error4.#^TOP_LEVEL_THROW2^#
