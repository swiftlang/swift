// RUN: %target-build-swift -emit-imported-modules -module-name imported_modules_underlying %s -o %t.importedmodules -import-underlying-module -I %S/Inputs/imported_modules/
// RUN: diff %t.importedmodules %S/Inputs/imported_modules/imported_modules_underlying.importedmodules

// REQUIRES: objc_interop
