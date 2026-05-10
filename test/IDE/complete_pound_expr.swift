// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_EXPR_1 | %FileCheck %s -check-prefix=POUND_EXPR_INTCONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_EXPR_2 | %FileCheck %s -check-prefix=POUND_EXPR_STRINGCONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_EXPR_3 | %FileCheck %s -check-prefix=POUND_EXPR_SELECTORCONTEXT
// REQUIRES: objc_interop, swift_swift_parser

import ObjectiveC

func useInt(_ str: Int) -> Bool {}
func useString(_ str: String) -> Bool {}
func useSelector(_ sel: Selector) -> Bool {}

func test1() {
  let _ = useInt(##^POUND_EXPR_1^#)
  let _ = useString(##^POUND_EXPR_2^#)
  let _ = useSelector(##^POUND_EXPR_3^#)
}

// POUND_EXPR_INTCONTEXT-NOT: warning
// POUND_EXPR_INTCONTEXT-NOT: error
// POUND_EXPR_INTCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: function[#ExpressibleByStringLiteral#]; name=function
// POUND_EXPR_INTCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: fileID[#ExpressibleByStringLiteral#]; name=fileID
// POUND_EXPR_INTCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: file[#ExpressibleByStringLiteral#]; name=file
// POUND_EXPR_INTCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: dsohandle[#UnsafeRawPointer#]; name=dsohandle
// POUND_EXPR_INTCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: column[#ExpressibleByIntegerLiteral#]; name=column
// POUND_EXPR_INTCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: line[#ExpressibleByIntegerLiteral#]; name=line
// POUND_EXPR_INTCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: externalMacro({#module: String#}, {#type: String#})[#T#]; name=externalMacro(module:type:)
// POUND_EXPR_INTCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: filePath[#ExpressibleByStringLiteral#]; name=filePath

// POUND_EXPR_STRINGCONTEXT-NOT: warning
// POUND_EXPR_STRINGCONTEXT-NOT: error
// POUND_EXPR_STRINGCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: function[#ExpressibleByStringLiteral#]; name=function
// POUND_EXPR_STRINGCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: fileID[#ExpressibleByStringLiteral#]; name=fileID
// POUND_EXPR_STRINGCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: file[#ExpressibleByStringLiteral#]; name=file
// POUND_EXPR_STRINGCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: dsohandle[#UnsafeRawPointer#]; name=dsohandle
// POUND_EXPR_STRINGCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: column[#ExpressibleByIntegerLiteral#]; name=column
// POUND_EXPR_STRINGCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: line[#ExpressibleByIntegerLiteral#]; name=line
// POUND_EXPR_STRINGCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: filePath[#ExpressibleByStringLiteral#]; name=filePath
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword/None/TypeRelation[Convertible]: keyPath({#@objc property sequence#})[#String#];
// POUND_EXPR_STRINGCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: externalMacro({#module: String#}, {#type: String#})[#T#]; name=externalMacro(module:type:)

// POUND_EXPR_SELECTORCONTEXT-NOT: warning
// POUND_EXPR_SELECTORCONTEXT-NOT: error
// POUND_EXPR_SELECTORCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: function[#ExpressibleByStringLiteral#]; name=function
// POUND_EXPR_SELECTORCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: fileID[#ExpressibleByStringLiteral#]; name=fileID
// POUND_EXPR_SELECTORCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: file[#ExpressibleByStringLiteral#]; name=file
// POUND_EXPR_SELECTORCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: dsohandle[#UnsafeRawPointer#]; name=dsohandle
// POUND_EXPR_SELECTORCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: column[#ExpressibleByIntegerLiteral#]; name=column
// POUND_EXPR_SELECTORCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: line[#ExpressibleByIntegerLiteral#]; name=line
// POUND_EXPR_SELECTORCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: filePath[#ExpressibleByStringLiteral#]; name=filePath
// POUND_EXPR_SELECTORCONTEXT-DAG: Keyword/None/TypeRelation[Convertible]: selector({#@objc method#})[#Selector#];
// POUND_EXPR_SELECTORCONTEXT-DAG: Decl[Macro]/OtherModule[Swift]/IsSystem: externalMacro({#module: String#}, {#type: String#})[#T#]; name=externalMacro(module:type:)
