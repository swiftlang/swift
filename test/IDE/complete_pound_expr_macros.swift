// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_EXPR_1 -enable-experimental-feature Macros| %FileCheck %s -check-prefix=POUND_EXPR_INTCONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_EXPR_2 -enable-experimental-feature Macros | %FileCheck %s -check-prefix=POUND_EXPR_STRINGCONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BARE_EXPR_1 -enable-experimental-feature Macros| %FileCheck %s -check-prefix=BARE_EXPR_INTCONTEXT
// REQUIRES: objc_interop

import ObjectiveC

macro myLine: Int = A.B
macro myFilename<T: ExpressiblyByStringLiteral>: T = A.B
macro myStringify<T>(_: T) -> (T, String) = A.B

struct Color { }

macro myColorLiteral(red: Float, green: Float, blue: Float, alpha: Float) -> Color = A.B

func useInt(_ str: Int) -> Bool {}
func useString(_ str: String) -> Bool {}

func test1() {
  let _ = useInt(##^POUND_EXPR_1^#)
  let _ = useString(##^POUND_EXPR_2^#)
  let _ = useInt(#^BARE_EXPR_1^#)
}

// POUND_EXPR_INTCONTEXT: Begin completions, 11 items
// POUND_EXPR_INTCONTEXT-DAG: Keyword[#function]/None:            function[#String#]; name=function
// POUND_EXPR_INTCONTEXT-DAG: Keyword[#file]/None:                file[#String#]; name=file
// POUND_EXPR_INTCONTEXT-DAG: Keyword[#fileID]/None: fileID[#String#]; name=fileID
// POUND_EXPR_INTCONTEXT-DAG: Keyword[#filePath]/None: filePath[#String#]; name=filePath
// POUND_EXPR_INTCONTEXT-DAG: Keyword[#line]/None/TypeRelation[Convertible]: line[#Int#]; name=line
// POUND_EXPR_INTCONTEXT-DAG: Keyword[#column]/None/TypeRelation[Convertible]: column[#Int#]; name=column
// POUND_EXPR_INTCONTEXT-DAG: Keyword[#dsohandle]/None:           dsohandle[#UnsafeRawPointer#]; name=dsohandle
// POUND_EXPR_INTCONTEXT-DAG: Decl[Macro]/CurrModule:             myFilename[#T#]; name=myFilename
// POUND_EXPR_INTCONTEXT-DAG: Decl[Macro]/CurrModule/TypeRelation[Convertible]: myLine[#Int#]; name=myLine
// POUND_EXPR_INTCONTEXT-DAG: Decl[Macro]/CurrModule:             myStringify({#T#})[#(T, String)#]; name=myStringify()
// POUND_EXPR_INTCONTEXT-DAG: Decl[Macro]/CurrModule:             myColorLiteral({#red: Float#}, {#green: Float#}, {#blue: Float#}, {#alpha: Float#})[#Color#]; name=myColorLiteral(red:green:blue:alpha:)

// POUND_EXPR_INTCONTEXT: End completions

// POUND_EXPR_STRINGCONTEXT: Begin completions, 12 items
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword[#function]/None/TypeRelation[Convertible]: function[#String#];
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword[#file]/None/TypeRelation[Convertible]: file[#String#];
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword[#fileID]/None/TypeRelation[Convertible]: fileID[#String#];
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword[#filePath]/None/TypeRelation[Convertible]: filePath[#String#];
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword[#line]/None:                line[#Int#];
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword[#column]/None:              column[#Int#];
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword[#dsohandle]/None:           dsohandle[#UnsafeRawPointer#];
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword/None/TypeRelation[Convertible]: keyPath({#@objc property sequence#})[#String#];
// POUND_EXPR_STRINGCONTEXT-DAG: Decl[Macro]/CurrModule:             myColorLiteral({#red: Float#}, {#green: Float#}, {#blue: Float#}, {#alpha: Float#})[#Color#]; name=myColorLiteral(red:green:blue:alpha:)
// POUND_EXPR_STRINGCONTEXT-DAG: Decl[Macro]/CurrModule:             myFilename[#T#]; name=myFilename
// POUND_EXPR_STRINGCONTEXT-DAG: Decl[Macro]/CurrModule:             myLine[#Int#]; name=myLine
// POUND_EXPR_STRINGCONTEXT-DAG: Decl[Macro]/CurrModule:             myStringify({#T#})[#(T, String)#]; name=myStringify()
// POUND_EXPR_STRINGCONTEXT: End completions

// BARE_EXPR_INTCONTEXT: Begin completions,
// BARE_EXPR_INTCONTEXT-DAG: Keyword[#fileID]/None:              #fileID[#String#]; name=#fileID
// BARE_EXPR_INTCONTEXT-DAG: Keyword[#file]/None:                #file[#String#]; name=#file
// BARE_EXPR_INTCONTEXT-DAG: Keyword[#filePath]/None:            #filePath[#String#]; name=#filePath
// BARE_EXPR_INTCONTEXT-DAG: Keyword[#function]/None:            #function[#String#]; name=#function
// BARE_EXPR_INTCONTEXT-DAG: Keyword[#line]/None/TypeRelation[Convertible]: #line[#Int#]; name=#line
// BARE_EXPR_INTCONTEXT-DAG: Keyword[#column]/None/TypeRelation[Convertible]: #column[#Int#]; name=#column
// BARE_EXPR_INTCONTEXT-DAG: Keyword[#dsohandle]/None:           #dsohandle[#UnsafeRawPointer#]; name=#dsohandle
// BARE_EXPR_INTCONTEXT-DAG: Decl[Macro]/CurrModule:             #myFilename[#T#]; name=#myFilename
// BARE_EXPR_INTCONTEXT-DAG: Decl[Struct]/CurrModule:            Color[#Color#]; name=Color
// BARE_EXPR_INTCONTEXT-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Invalid]: test1()[#Void#]; name=test1()
// BARE_EXPR_INTCONTEXT-DAG: Decl[Macro]/CurrModule/TypeRelation[Convertible]: #myLine[#Int#]; name=#myLine
// BARE_EXPR_INTCONTEXT-DAG: Decl[FreeFunction]/CurrModule:      useInt({#(str): Int#})[#Bool#]; name=useInt(:)
// BARE_EXPR_INTCONTEXT-DAG: Decl[Macro]/CurrModule:             #myStringify({#T#})[#(T, String)#]; name=#myStringify()
// BARE_EXPR_INTCONTEXT-DAG: Decl[FreeFunction]/CurrModule:      useString({#(str): String#})[#Bool#]; name=useString(:)
// BARE_EXPR_INTCONTEXT-DAG: Decl[Macro]/CurrModule:             #myColorLiteral({#red: Float#}, {#green: Float#}, {#blue: Float#}, {#alpha: Float#})[#Color#]; name=#myColorLiteral(red:green:blue:alpha:)
// BARE_EXPR_INTCONTEXT: End completions
