
// Also declared in modules ExportsAC and A.
precedencegroup ShadowsModulesAC {}

// Also declared in modules A and ExportsAC.
infix operator ?????

// Also declared in modules A and ExportsAC, with a different precedencegroup.
infix operator ?????? : ShadowsModulesAC
