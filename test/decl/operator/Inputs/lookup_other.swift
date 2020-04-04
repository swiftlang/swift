
@_exported import D

// Both of these decls are also declared in module B, but we should prefer
// these decls.
precedencegroup DeclaredAcrossFiles {
  higherThan: AssignmentPrecedence
  associativity: left
}
infix operator &&& : DeclaredAcrossFiles

// Declared in both modules A and B, but shadowed by this decl.
precedencegroup DeclaredInModulesABShadowed {}
