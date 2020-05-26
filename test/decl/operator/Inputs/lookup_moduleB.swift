
// Both of these decls are also declared in lookup_other, and we should prefer
// those decls.
precedencegroup DeclaredAcrossFiles {}
infix operator &&&

// Also declared in module A.
infix operator ???
precedencegroup DeclaredInModulesAB {}

// Also declared in module A, but with a different precedence group.
infix operator ????

// Declared in both modules A and B, but shadowed by lookup_other.
precedencegroup DeclaredInModulesABShadowed {}

// Also declared in module A.
postfix operator <?

// Declared in both modules A and B, and also the stdlib.
precedencegroup TernaryPrecedence {}

// These are both also declared in module ExportsAC.
infix operator ???!
precedencegroup DeclaredInModulesBExportsAC {}

// Also declared in module ExportsAC, but with a different precedence group.
infix operator ????!

// Also declared in module ExportsAC.
postfix operator <!
