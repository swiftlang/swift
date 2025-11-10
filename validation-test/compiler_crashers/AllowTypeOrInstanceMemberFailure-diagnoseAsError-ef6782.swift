// {"kind":"typecheck","original":"4d90fbe2","signature":"swift::constraints::AllowTypeOrInstanceMemberFailure::diagnoseAsError()","signatureAssert":"Assertion failed: (TypeDC->getContextKind() == DeclContextKind::AbstractFunctionDecl && \"Expected function decl context for initializer!\"), function diagnoseAsError","splits":[0]}
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: not --crash %target-swift-frontend -typecheck %t/main.swift %t/x1.swift
//--- main.swift

//--- x1.swift
let <#pattern#>: LazyMapCollection = .lazy
