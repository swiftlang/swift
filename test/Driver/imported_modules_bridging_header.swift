// RUN: %target-build-swift -emit-imported-modules %s -o %t.importedmodules -import-objc-header %S/Inputs/imported_modules/bridging_header.h -I %S/Inputs/imported_modules
// RUN: diff %t.importedmodules %S/Inputs/imported_modules/imported_modules_bridging_header.importedmodules
