// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CATCH1 | FileCheck %s -check-prefix=CATCH1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=THROW1 | FileCheck %s -check-prefix=THROW1

protocol ErrorPro1 : _ErrorType {}
class Error1 : _ErrorType {}
class Error2 : _ErrorType {}
class NoneError1 {}

{
  do {} catch #^CATCH1^#

// CATCH1:      Begin completions
// CATCH1-NOT:  Decl[Class]/CurrModule:             NoneError1[#NoneError1#]; name=NoneError1{{$}}
// CATCH1-DAG:  Decl[Class]/CurrModule:             Error2[#Error2#]; name=Error2{{$}}
// CATCH1-DAG:  Decl[Class]/CurrModule:             Error1[#Error1#]; name=Error1{{$}}
// CATCH1-DAG:  Keyword/None:                       let e {|}; name=e{{$}}
// CATCH1-DAG:  Keyword/None:                       _ {|}; name=_{{$}}
}

{
  let text = "NonError"
  let e1 = Error1()
  let e2 = Error2()
  throw #^THROW1^#

// THROW1:      Begin completions
// THROW1-NOT:  Decl[LocalVar]/Local:               text[#String#]; name=text{{$}}
// THROW1-DAG:  Decl[LocalVar]/Local:               e1[#Error1#]; name=e1{{$}}
// THROW1-DAG:  Decl[LocalVar]/Local:               e2[#Error2#]; name=e2{{$}}
// THROW1-DAG:  Decl[Class]/CurrModule:             Error2[#Error2#]; name=Error2{{$}}
// THROW1-DAG:  Decl[Class]/CurrModule:             Error1[#Error1#]; name=Error1{{$}}
// THROW1-NOT:  Decl[Protocol]/CurrModule:          ErrorPro1[#ErrorPro1#]; name=ErrorPro1{{$}}
// THROW1-NOT:  Decl[Class]/CurrModule:             NoneError1[#NoneError1#]; name=NoneError1{{$}}
}
