// RUN: %empty-directory(%t)

// -parse-as-library added so that the PDB isn't added to a TopLevelCodeDecl,
// which isn't serialized at all
// RUN: %target-swift-frontend -emit-module -o %t/errors.swiftmodule -module-name errors -experimental-allow-module-with-compiler-errors -parse-as-library %s

let self = 1
