" Vim syntax file
" Language: sil

if exists("b:current_syntax")
    finish
endif

syn keyword swiftImport import skipwhite nextgroup=swiftImportModule
syn match swiftImportModule /\<[A-Za-z_][A-Za-z_0-9]*\>/ contained nextgroup=swiftImportComponent
syn match swiftImportComponent /\.\<[A-Za-z_][A-Za-z_0-9]*\>/ contained nextgroup=swiftImportComponent

syn match swiftLineComment   /^#!.*/
syn match swiftTypeName  /\<[A-Z][a-zA-Z_0-9]*\>/
syn match swiftDecimal /\<[-]\?[0-9]\+\>/
syn match swiftDecimal /\<[-+]\?[0-9]\+\>/

syn match swiftTypeName /\$\*\<\?[A-Z][a-zA-Z0-9_]*\>/
syn match swiftVarName /%\<[A-z[a-z_0-9]\+\(#[0-9]\+\)\?\>/

syn keyword swiftKeyword break case continue default do else for if in static switch return where while skipwhite

syn keyword swiftKeyword sil internal thunk skipwhite
syn keyword swiftKeyword public hidden private shared public_external hidden_external skipwhite
syn keyword swiftKeyword getter setter allocator initializer enumelt destroyer globalaccessor objc skipwhite
syn keyword swiftKeyword alloc_stack alloc_ref alloc_ref_dynamic alloc_box dealloc_stack dealloc_box dealloc_ref skipwhite
syn keyword swiftKeyword debug_value debug_value_addr skipwhite
syn keyword swiftKeyword load store assign  mark_uninitialized mark_function_escape copy_addr destroy_addr index_addr index_raw_pointer to skipwhite
syn keyword swiftKeyword strong_retain strong_retain_autoreleased strong_release strong_retain_unowned ref_to_unowned unowned_to_ref unowned_retain unowned_release load_weak store_weak fix_lifetime skipwhite
syn keyword swiftKeyword function_ref integer_literal float_literal string_literal global_addr skipwhite
syn keyword swiftKeyword class_method super_method witness_method dynamic_method skipwhite
syn keyword swiftKeyword apply partial_apply builtin skipwhite
syn keyword swiftKeyword metatype value_metatype existential_metatype skipwhite
syn keyword swiftKeyword retain_value release_value tuple tuple_extract tuple_element_addr struct struct_extract struct_element_addr ref_element_addr skipwhite
syn keyword swiftKeyword init_enum_data_addr unchecked_enum_data unchecked_take_enum_data_addr inject_enum_addr skipwhite
syn keyword swiftKeyword init_existential_addr deinit_existential_addr open_existential_addr init_existential_ref open_existential_ref skipwhite
syn keyword swiftKeyword upcast address_to_pointer pointer_to_address unchecked_addr_cast unchecked_ref_cast ref_to_raw_pointer raw_pointer_to_ref convert_function thick_to_objc_metatype objc_to_thick_metatype thin_to_thick_function is_nonnull unchecked_ref_bit_cast unchecked_trivial_bit_cast skipwhite
syn keyword swiftKeyword unconditional_checked_cast skipwhite
syn keyword swiftKeyword cond_fail skipwhite
syn keyword swiftKeyword unreachable return autorelease_return br cond_br switch_value select_value switch_enum switch_enum_addr dynamic_method_br checked_cast_br skipwhite
syn keyword swiftKeyword project_block_storage init_block_storage_header copy_block skipwhite

syn keyword swiftTypeDefinition class extension protocol struct typealias enum skipwhite nextgroup=swiftTypeName
syn region swiftTypeAttributes start="\[" end="\]" skipwhite contained nextgroup=swiftTypeName
syn match swiftTypeName /\<[A-Za-z_][A-Za-z_0-9\.]*\>/ contained nextgroup=swiftTypeParameters

syn region swiftTypeParameters start="<" end=">" skipwhite contained

syn keyword swiftFuncDefinition func skipwhite nextgroup=swiftFuncAttributes,swiftFuncName,swiftOperator
syn region swiftFuncAttributes start="\[" end="\]" skipwhite contained nextgroup=swiftFuncName,swiftOperator
syn match swiftFuncName /\<[A-Za-z_][A-Za-z_0-9]*\>/ skipwhite contained nextgroup=swiftTypeParameters
syn keyword swiftFuncKeyword subscript init destructor nextgroup=swiftTypeParameters

syn keyword swiftVarDefinition var skipwhite nextgroup=swiftVarName
syn keyword swiftVarDefinition let skipwhite nextgroup=swiftVarName
syn match swiftVarName /\<[A-Za-z_][A-Za-z_0-9]*\>/ skipwhite contained

syn keyword swiftDefinitionModifier static

syn match swiftImplicitVarName /\$\<[A-Za-z_0-9]\+\>/

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
hi def link swiftLabel Label
hi def link swiftNew Operator

let b:current_syntax = "sil"
