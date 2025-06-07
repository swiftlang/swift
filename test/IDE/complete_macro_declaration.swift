// REQUIRES: swift_swift_parser
// RUN: %batch-code-completion

let globalVar = 1
macro expect(file: Int = #^DEFAULT_ARG^#) = #externalMacro(module: "MyModule", type: "MyMacro")
// DEFAULT_ARG: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: globalVar[#Int#]; name=globalVar

@freestanding(expression)
macro otherExternalMacro() = ##^EXTERNAL_MACRO^#
// EXTERNAL_MACRO: Decl[Macro]/OtherModule[Swift]/IsSystem: externalMacro({#module: String#}, {#type: String#})[#T#]; name=externalMacro(module:type:)

@freestanding(expression)
macro externalMacroWithTrailing() = ##^EXTERNAL_MACRO_WITH_TRAILING?check=EXTERNAL_MACRO^#externalMacro

@freestanding(expression)
macro externalMacroCallPattern() = #externalMacro(#^EXTERNAL_MACRO_CALL_PATTERN^#)
// EXTERNAL_MACRO_CALL_PATTERN: Pattern/None/Flair[ArgLabels]/TypeRelation[Convertible]: ['(']{#module: String#}, {#type: String#}[')'][#Void#]; name=module:type:

@freestanding(expression)
macro externalMacroCallPattern() = #externalMacro(module: "MyModule", #^EXTERNAL_MACRO_TYPE_ARG_LABEL^#)
// EXTERNAL_MACRO_TYPE_ARG_LABEL: Pattern/Local/Flair[ArgLabels]:     {#type: String#}[#String#]; name=type:
