// RUN: %empty-directory(%t)
//
// RUN: %target-clang -fobjc-arc %S/../Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: mkdir -p %t/ObjCClasses.framework/Headers
// RUN: cp %S/../Inputs/ObjCClasses/ObjCClasses.h %t/ObjCClasses.framework/Headers
// RUN: mkdir -p %t/ObjCClasses.framework/Modules
// RUN: cp %S/../Inputs/ObjCClasses/framework.module.map %t/ObjCClasses.framework/Modules/module.modulemap
// RUN: %target-clang -dynamiclib -fobjc-arc -fmodules %t/ObjCClasses.o -o %t/ObjCClasses.framework/ObjCClasses -install_name @executable_path/ObjCClasses.framework/ObjCClasses
// RUN: %target-codesign %t/ObjCClasses.framework/ObjCClasses

// RUN: %target-build-swift -F %t %s -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/ObjCClasses.framework

// REQUIRES: executable_test
// REQUIRES: objc_interop

// Make sure that functionally if we refer to an objc class via autolinking such
// that the class is not needed by the module directly, we still emit an objc
// class ref to ensure that the static linker links against the autolinked
// library.

import ObjCClasses

func main() {
  let a = Animal()
  print(a)
}

main()
