// RUN: %empty-directory(%t/batch-code-completion) 
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t/batch-code-completion

// REQUIRES: objc_interop

import Foundation // importer SDK

protocol ErrorPro1 : Error {}
class Error1 : Error {}
class Error2 : Error {}
class Error3 {}
extension Error3 : Error{}
enum Error4 : Error {
  case E1
  case E2(Int32)
}
class NoneError1 {}

func getError1() -> Error1 { return Error1() }
func getNSError() -> NSError { return NSError(domain: "", code: 1, userInfo: [:]) }

func test001() {
  do {} catch #^CATCH1^#

// CATCH1-DAG:  Decl[Enum]/CurrModule/TypeRelation[Convertible]: Error4[#Error4#]; name=Error4{{$}}
// CATCH1-DAG:  Decl[Class]/CurrModule/TypeRelation[Convertible]: Error3[#Error3#]; name=Error3{{$}}
// CATCH1-DAG:  Decl[Class]/CurrModule/TypeRelation[Convertible]: Error2[#Error2#]; name=Error2{{$}}
// CATCH1-DAG:  Decl[Class]/CurrModule/TypeRelation[Convertible]: Error1[#Error1#]; name=Error1{{$}}
// CATCH1-DAG:  Keyword[let]/None:                  let{{; name=.+$}}
// CATCH1-DAG:  Decl[Class]/CurrModule:             NoneError1[#NoneError1#]; name=NoneError1{{$}}
// CATCH1-DAG:  Decl[Class]/OtherModule[Foundation]/IsSystem: NSError[#NSError#]{{; name=.+$}}
}

func test002() {
  let text = "NonError"
  let e1 = Error1()
  let e2 = Error2()
  throw #^THROW1?check=THROW1,THROW1-LOCAL^#

// THROW1-DAG:  Decl[Enum]/CurrModule/TypeRelation[Convertible]:              Error4[#Error4#]; name=Error4{{$}}
// THROW1-DAG:  Decl[Class]/CurrModule/TypeRelation[Convertible]:             Error3[#Error3#]; name=Error3{{$}}
// THROW1-DAG:  Decl[Class]/CurrModule/TypeRelation[Convertible]:             Error2[#Error2#]; name=Error2{{$}}
// THROW1-DAG:  Decl[Class]/CurrModule/TypeRelation[Convertible]:             Error1[#Error1#]; name=Error1{{$}}
// THROW1-DAG:  Decl[Protocol]/CurrModule/Flair[RareType]/TypeRelation[Convertible]: ErrorPro1[#ErrorPro1#]; name=ErrorPro1{{$}}
// THROW1-DAG:  Decl[FreeFunction]/CurrModule/TypeRelation[Convertible]:      getError1()[#Error1#]{{; name=.+$}}
// THROW1-DAG:  Decl[FreeFunction]/CurrModule/TypeRelation[Convertible]:      getNSError()[#NSError#]{{; name=.+$}}
// THROW1-DAG:  Decl[Class]/CurrModule:             NoneError1[#NoneError1#]; name=NoneError1{{$}}

// THROW1-LOCAL-DAG: Decl[LocalVar]/Local:                           text[#String#]; name=text{{$}}
// THROW1-LOCAL-DAG: Decl[LocalVar]/Local/TypeRelation[Convertible]: e1[#Error1#]; name=e1{{$}}
// THROW1-LOCAL-DAG: Decl[LocalVar]/Local/TypeRelation[Convertible]: e2[#Error2#]; name=e2{{$}}
}

func test003() {
  do {} catch Error4.#^CATCH2^#
// CATCH2-DAG: Decl[EnumElement]/CurrNominal: E1[#Error4#]{{; name=.+$}}
// CATCH2-DAG: Decl[EnumElement]/CurrNominal: E2({#Int32#})[#Error4#]{{; name=.+$}}
}

func test004() {
  throw Error4.#^THROW2^#
// THROW2-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Convertible]: E1[#Error4#]{{; name=.+$}}
// THROW2-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Convertible]: E2({#Int32#})[#Error4#]{{; name=.+$}}
}

func test005() {
  do {} catch Error4.E2#^CATCH3^#
// CATCH3-DAG: Pattern/CurrModule/Flair[ArgLabels]:               ({#Int32#})[#Error4#]{{; name=.+$}}
}

func testInvalid() {
  try throw Error4.#^THROW3^#
// THROW3-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Convertible]:      E1[#Error4#]{{; name=.+$}}
// THROW3-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Convertible]:      E2({#Int32#})[#Error4#]{{; name=.+$}}
}

//===--- Top-level throw/catch
do {} catch #^TOP_LEVEL_CATCH1?check=CATCH1^# {}
throw #^TOP_LEVEL_THROW1?check=THROW1^#
do {} catch Error4.#^TOP_LEVEL_CATCH2?check=CATCH2^# {}
throw Error4.#^TOP_LEVEL_THROW2?check=THROW2^#
try throw Error4.#^TOP_LEVEL_THROW3?check=THROW3^#

//===--- Inside catch body

// Statement-level code completions. This isn't exhaustive.
// STMT-DAG: Keyword[if]/None:                       if; name=if
// STMT-DAG: Decl[Class]/CurrModule:             Error1[#Error1#]; name=Error1
// STMT-DAG: Decl[Class]/CurrModule:             Error2[#Error2#]; name=Error2
// STMT-DAG: Decl[FreeFunction]/CurrModule:      getError1()[#Error1#]; name=getError1()
// STMT-DAG: Decl[FreeFunction]/CurrModule:      getNSError()[#NSError#]; name=getNSError()

func test006() {
  do {
  } catch {
    #^INSIDE_CATCH1?check=STMT,IMPLICIT_ERROR^#
  }
// IMPLICIT_ERROR-DAG: Decl[LocalVar]/Local:  error[#any Error#]; name=error
}
func test007() {
  do {
  } catch let e {
    #^INSIDE_CATCH2?check=STMT,EXPLICIT_ERROR_E^#
  }
// EXPLICIT_ERROR_E-DAG: Decl[LocalVar]/Local: e[#any Error#]; name=e
}
func test008() {
  do {
  } catch let e as NSError {
    #^INSIDE_CATCH3?check=STMT,EXPLICIT_NSERROR_E^#
  }
// EXPLICIT_NSERROR_E-DAG: Decl[LocalVar]/Local: e[#NSError#]; name=e
}
func test009() {
  do {
  } catch Error4.E2(let i) {
    #^INSIDE_CATCH4?check=STMT,EXPLICIT_ERROR_PAYLOAD_I^#
  }
// EXPLICIT_ERROR_PAYLOAD_I-DAG: Decl[LocalVar]/Local: i[#Int32#]; name=i
}
func test010() {
  do {
  } catch let awesomeError {
  } catch let e {
    #^INSIDE_CATCH5?check=STMT,EXPLICIT_ERROR_E;check=NO_ERROR_AND_A^#
  } catch {}
// NO_ERROR_AND_A-NOT: awesomeError
// NO_ERROR_AND_A-NOT: Decl[LocalVar]/Local: error
}
func test011() {
  do {
  } catch let awesomeError {
  } catch let excellentError {
  } catch {}
  #^INSIDE_CATCH6?check=STMT;check=NO_ERROR_AND_A,NO_E^#
// NO_E-NOT: excellentError
}
func test012() {
  do {
  } catch {
    error.#^INSIDE_CATCH_ERR_DOT1?check=ERROR_DOT^#
  }
}
// ERROR_DOT-DAG: Keyword[self]/CurrNominal: self[#any Error#]; name=self
func test013() {
  do {
  } catch let e {
    e.#^INSIDE_CATCH_ERR_DOT2?check=ERROR_DOT^#
  }
}
func test014() {
  do {
  } catch let e as NSError {
    e.#^INSIDE_CATCH_ERR_DOT3?check=NSERROR_DOT^#
  }
// NSERROR_DOT-DAG: Decl[InstanceVar]/CurrNominal/IsSystem: domain[#String#]; name=domain
// NSERROR_DOT-DAG: Decl[InstanceVar]/CurrNominal/IsSystem: code[#Int#]; name=code
// NSERROR_DOT-DAG: Decl[InstanceVar]/Super/IsSystem:       hashValue[#Int#]; name=hashValue
// NSERROR_DOT-DAG: Decl[InstanceMethod]/Super/IsSystem:    myClass()[#AnyClass!#]; name=myClass()
// NSERROR_DOT-DAG: Decl[InstanceMethod]/Super/IsSystem:    isEqual({#(other): NSObject!#})[#Bool#]; name=isEqual(:)
// NSERROR_DOT-DAG: Decl[InstanceVar]/Super/IsSystem:       hash[#Int#]; name=hash
// NSERROR_DOT-DAG: Decl[InstanceVar]/Super/IsSystem:       description[#String#]; name=description
}
func test015() {
  do {
  } catch Error4.E2(let i) where i == 2 {
    i.#^INSIDE_CATCH_ERR_DOT4?check=INT_DOT^#
  }
}
// Check that we can complete on the bound value; Not exhaustive..
// INT_DOT-DAG: Decl[InstanceVar]/Super/IsSystem: bigEndian[#Int32#]; name=bigEndian
// INT_DOT-DAG: Decl[InstanceVar]/Super/IsSystem: littleEndian[#Int32#]; name=littleEndian

//===--- Inside catch body top-level
do {
} catch {
  #^TOP_LEVEL_INSIDE_CATCH1?check=STMT,IMPLICIT_ERROR^#
}
do {
} catch {
  error.#^TOP_LEVEL_INSIDE_CATCH_ERR_DOT1?check=ERROR_DOT^#
}

func canThrowError4() throws(Error4) {}
func test016() {
  do {
    try canThrowError4()
  } catch .E2(let i) {
    i.#^INSIDE_CATCH_TYPEDERR_DOT?check=INT_DOT^#
  }
}
