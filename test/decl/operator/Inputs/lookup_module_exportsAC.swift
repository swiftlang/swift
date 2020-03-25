
@_exported import A
@_exported import C

// Also declared in module A, but this decl shadows it.
precedencegroup ShadowsModuleA {}

// Also declared in modules A and C, but this decl shadows it.
precedencegroup ShadowsModulesAC {}

// Also declared in modules A and C, but this decl shadows it.
infix operator ?????

// Also declared in modules A and C, but with a different precedencegroup.
// However that shouldn't impact shadowing.
infix operator ??????

// These are both also declared in module B.
infix operator ???!
precedencegroup DeclaredInModulesBExportsAC {}

// Also declared in module B, but with a different precedence group.
infix operator ????! : ShadowsModuleA

// Also declared in module B.
postfix operator <!
