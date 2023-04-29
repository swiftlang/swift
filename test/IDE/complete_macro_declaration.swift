// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

let globalVar = 1
macro expect(file: Int = #^DEFAULT_ARG^#) = #externalMacro(module: "MyModule", type: "MyMacro")

// DEFAULT_ARG: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: globalVar[#Int#]; name=globalVar
