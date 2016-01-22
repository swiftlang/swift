" Vim syntax file
" Language: swift
" Maintainer: Joe Groff <jgroff@apple.com>
" Last Change: 2013 Feb 2

if exists("b:current_syntax")
    finish
endif

syn keyword swiftImport import skipwhite nextgroup=swiftImportModule

syn match swiftImportModule /\<[A-Za-z_][A-Za-z_0-9]*\>/ contained nextgroup=swiftImportComponent
syn match swiftImportComponent /\.\<[A-Za-z_][A-Za-z_0-9]*\>/ contained nextgroup=swiftImportComponent

syn keyword swiftKeyword break case continue default do else for if in static switch repeat return where while public internal private mutating nonmutating var let typealias protocol extension skipwhite
syn keyword swiftKeyword associatedtype override required convenience final defer skipwhite

syn keyword swiftTypeDefinition class extension protocol struct typealias enum skipwhite nextgroup=swiftTypeName
syn region swiftTypeAttributes start="\[" end="\]" skipwhite contained nextgroup=swiftTypeName
syn match swiftTypeName /\<[A-Za-z_][A-Za-z_0-9\.]*\>/ contained nextgroup=swiftTypeParameters

syn region swiftTypeParameters start="<" end=">" skipwhite contained

syn keyword swiftMutating mutating skipwhite nextgroup=swiftFuncDefinition
syn keyword swiftFuncDefinition func skipwhite nextgroup=swiftFuncAttributes,swiftFuncName,swiftOperator
syn region swiftFuncAttributes start="\[" end="\]" skipwhite contained nextgroup=swiftFuncName,swiftOperator
syn match swiftFuncName /\<[A-Za-z_][A-Za-z_0-9]*\>/ skipwhite contained nextgroup=swiftTypeParameters
syn keyword swiftFuncKeyword subscript init destructor nextgroup=swiftTypeParameters

syn keyword swiftVarDefinition var skipwhite nextgroup=swiftVarName
syn keyword swiftVarDefinition let skipwhite nextgroup=swiftVarName
syn match swiftVarName /\<[A-Za-z_][A-Za-z_0-9]*\>/ skipwhite contained

syn keyword swiftDefinitionModifier static public internal private

syn match swiftImplicitVarName /\$\<[A-Za-z_0-9]\+\>/

syn match swiftTypeDeclaration /:/ nextgroup=swiftTypeAttributes,swiftTypeName skipwhite
syn match swiftTypeDeclaration /->/ nextgroup=swiftTypeAttributes,swiftTypeName skipwhite

syn keyword swiftIdentifierKeyword metatype super self Self

syn keyword swiftNew new skipwhite nextgroup=swiftTypeName

syn keyword swiftBoolean true false

syn region swiftString start=/"/ skip=/\\\\\|\\"/ end=/"/ contains=swiftInterpolation
syn region swiftInterpolation start=/\\(/ end=/)/ contained
syn region swiftComment start="/\*" end="\*/" contains=swiftComment,swiftLineComment,swiftTodo
syn region swiftLineComment start="//" end="$" contains=swiftComment,swiftTodo

syn match swiftDecimal /[+\-]\?\<\([0-9][0-9_]*\)\([.][0-9_]*\)\?\([eE][+\-]\?[0-9][0-9_]*\)\?\>/
syn match swiftHex /[+\-]\?\<0x[0-9A-Fa-f][0-9A-Fa-f_]*\(\([.][0-9A-Fa-f_]*\)\?[pP][+\-]\?[0-9][0-9_]*\)\?\>/
syn match swiftOct /[+\-]\?\<0o[0-7][0-7_]*\>/
syn match swiftBin /[+\-]\?\<0b[01][01_]*\>/

syn match swiftOperator +\.\@<!\.\.\.\@!\|[/=\-+*%<>!&|^~]\@<!\(/[/*]\@![/=\-+*%<>!&|^~]*\|*/\@![/=\-+*%<>!&|^~]*\|->\@![/=\-+*%<>!&|^~]*\|[=+%<>!&|^~][/=\-+*%<>!&|^~]*\)+ skipwhite nextgroup=swiftTypeParameters
syn match swiftOperator "\.\.[<.]" skipwhite nextgroup=swiftTypeParameters

syn match swiftChar /'\([^'\\]\|\\\(["'tnr0\\]\|x[0-9a-fA-F]\{2}\|u[0-9a-fA-F]\{4}\|U[0-9a-fA-F]\{8}\)\)'/

syn keyword swiftLabel get set

syn match swiftPreproc /^#\<if\>\|^#\<endif\>/

syn match swiftAttribute /@\<\w\+\>/ skipwhite

syn keyword swiftTodo TODO FIXME contained
syn keyword swiftNil nil
syn match swiftNilOps "\<as?\?\>"
syn match swiftNilOps "??"

hi def link swiftImport Include
hi def link swiftImportModule Title
hi def link swiftImportComponent Identifier
hi def link swiftKeyword Statement
hi def link swiftTypeDefinition Define
hi def link swiftTypeName Type
hi def link swiftTypeParameters Special
hi def link swiftTypeAttributes PreProc
hi def link swiftFuncDefinition Define
hi def link swiftDefinitionModifier Define
hi def link swiftFuncName Function
hi def link swiftFuncAttributes PreProc
hi def link swiftFuncKeyword Function
hi def link swiftVarDefinition Define
hi def link swiftVarName Identifier
hi def link swiftImplicitVarName Identifier
hi def link swiftIdentifierKeyword Identifier
hi def link swiftTypeDeclaration Delimiter
hi def link swiftBoolean Boolean
hi def link swiftString String
hi def link swiftInterpolation Special
hi def link swiftComment Comment
hi def link swiftLineComment Comment
hi def link swiftDecimal Number
hi def link swiftHex Number
hi def link swiftOct Number
hi def link swiftBin Number
hi def link swiftOperator Function
hi def link swiftChar Character
hi def link swiftLabel Operator
hi def link swiftNew Operator
hi def link swiftMutating Statement
hi def link swiftPreproc PreCondit
hi def link swiftAttribute Type
hi def link swiftTodo Todo
hi def link swiftNil Constant
hi def link swiftNilOps Operator

let b:current_syntax = "swift"
