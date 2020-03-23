
// Only declared in module A.
prefix operator >>>
public prefix func >>> (rhs: Int) {}

precedencegroup DeclaredInModuleA {}

// Declared in both modules A and B.
infix operator ???
precedencegroup DeclaredInModulesAB {}

// Declared in both modules A and B, but with a different
// precedence group in each.
infix operator ???? : DeclaredInModuleA

// Declared in both modules A and B, but shadowed by lookup_other.
precedencegroup DeclaredInModulesABShadowed {}

// Declared in both modules A and B.
postfix operator <?

// Declared in both modules A and B, and also the stdlib.
precedencegroup TernaryPrecedence {}

// Also declared in module 'ExportsAC', which shadows this decl.
precedencegroup ShadowsModuleA {}

// Also declared in modules ExportsAC and C.
precedencegroup ShadowsModulesAC {}

// Also declared in modules C and ExportsAC.
infix operator ?????

// Also declared in modules C and ExportsAC, but with a different
// precedencegroup.
infix operator ?????? : DeclaredInModuleA
