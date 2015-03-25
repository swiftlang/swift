// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CATCH1 | FileCheck %s -check-prefix=CATCH1

class Error1 : _ErrorType {}
class Error2 : _ErrorType {}

do {} catch #^CATCH1^#
// CATCH1:      Begin completions
// CATCH1-DAG:  Decl[Class]/CurrModule:             Error2[#Error2#]; name=Error2{{$}}
// CATCH1-DAG:  Decl[Class]/CurrModule:             Error1[#Error1#]; name=Error1{{$}}
