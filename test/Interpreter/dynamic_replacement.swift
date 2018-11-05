// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/libModule1.%target-dylib-extension) -DMODULE -module-name Module1 -emit-module -emit-module-path %t/Module1.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift-dylib(%t/libModule2.%target-dylib-extension) -I%t -L%t -lModule1 -Xlinker -rpath -Xlinker %t -DMODULE2 -module-name Module2 -emit-module -emit-module-path %t/Module2.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift -I%t -L%t -lModule1 -DMAIN -o %t/main -Xlinker -rpath -Xlinker %t %s -swift-version 5
// RUN: %target-codesign %t/main %t/libModule1.%target-dylib-extension %t/libModule2.%target-dylib-extension
// RUN: %target-run %t/main %t/libModule1.%target-dylib-extension %t/libModule2.%target-dylib-extension

// Now the same in optimized mode.
// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/libModule1.%target-dylib-extension) -O -DMODULE -module-name Module1 -emit-module -emit-module-path %t/Module1.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift-dylib(%t/libModule2.%target-dylib-extension) -O -I%t -L%t -lModule1 -Xlinker -rpath -Xlinker %t -DMODULE2 -module-name Module2 -emit-module -emit-module-path %t/Module2.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift -O -I%t -L%t -lModule1 -DMAIN -o %t/main -Xlinker -rpath -Xlinker %t %s -swift-version 5
// RUN: %target-codesign %t/main %t/libModule1.%target-dylib-extension %t/libModule2.%target-dylib-extension
// RUN: %target-run %t/main %t/libModule1.%target-dylib-extension %t/libModule2.%target-dylib-extension

// Now the same in size mode.
// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/libModule1.%target-dylib-extension) -Osize -DMODULE -module-name Module1 -emit-module -emit-module-path %t/Module1.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift-dylib(%t/libModule2.%target-dylib-extension) -Osize -I%t -L%t -lModule1 -Xlinker -rpath -Xlinker %t -DMODULE2 -module-name Module2 -emit-module -emit-module-path %t/Module2.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift -Osize -I%t -L%t -lModule1 -DMAIN -o %t/main -Xlinker -rpath -Xlinker %t %s -swift-version 5
// RUN: %target-codesign %t/main %t/libModule1.%target-dylib-extension %t/libModule2.%target-dylib-extension
// RUN: %target-run %t/main %t/libModule1.%target-dylib-extension %t/libModule2.%target-dylib-extension

// Now the same in optimized wholemodule mode.
// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/libModule1.%target-dylib-extension) -O -wmo -DMODULE -module-name Module1 -emit-module -emit-module-path %t/Module1.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift-dylib(%t/libModule2.%target-dylib-extension) -O -wmo -I%t -L%t -lModule1 -Xlinker -rpath -Xlinker %t -DMODULE2 -module-name Module2 -emit-module -emit-module-path %t/Module2.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift -O -wmo -I%t -L%t -lModule1 -DMAIN -o %t/main -Xlinker -rpath -Xlinker %t %s -swift-version 5
// RUN: %target-codesign %t/main %t/libModule1.%target-dylib-extension %t/libModule2.%target-dylib-extension
// RUN: %target-run %t/main %t/libModule1.%target-dylib-extension %t/libModule2.%target-dylib-extension


// REQUIRES: executable_test

import Module1

import StdlibUnittest

#if os(Linux)
  import Glibc
  let dylibSuffix = "so"
#else
  import Darwin
  let dylibSuffix = "dylib"
#endif

var DynamicallyReplaceable = TestSuite("DynamicallyReplaceable")

func expectedResult(_ forOriginalLibrary: Bool,  _ expectedOriginalString: String) -> String {
  if forOriginalLibrary {
    return expectedOriginalString
  } else {
    return "replacement of \(expectedOriginalString)"
	}
}

func checkExpectedResults(forOriginalLibrary useOrig: Bool) {
 expectTrue(public_global_func() ==
            expectedResult(useOrig, "public_global_func"))
 expectTrue(public_global_generic_func(Int.self) ==
            expectedResult(useOrig, "public_global_generic_func"))

 expectTrue(PublicClass().function() ==
            expectedResult(useOrig, "public_class_func"))
 expectTrue(PublicClass().genericFunction(Int.self) ==
            expectedResult(useOrig, "public_class_generic_func"))

 expectTrue(PublicStruct().function() ==
            expectedResult(useOrig, "public_struct_func"))
 expectTrue(PublicStruct().genericFunction(Int.self) ==
            expectedResult(useOrig, "public_struct_generic_func"))
 expectTrue(PublicStruct().public_stored_property ==
            expectedResult(useOrig, "public_stored_property"))
 expectTrue(PublicStruct()[0] ==
            expectedResult(useOrig, "public_subscript_get"))
 expectTrue(PublicStruct()[y:0] ==
            expectedResult(useOrig, "public_subscript_get_modify_read"))
 var testSetter = PublicStruct()
 testSetter[0] = "public_subscript_set"
 expectTrue(testSetter.str ==
            expectedResult(useOrig, "public_subscript_set"))
 testSetter[y:0] = "public_subscript_set_modify_read"
 expectTrue(testSetter.str ==
            expectedResult(useOrig, "public_subscript_set_modify_read"))

 expectTrue(PublicEnumeration<Int>.A.function() ==
            expectedResult(useOrig, "public_enum_func"))
 expectTrue(PublicEnumeration<Int>.B.genericFunction(Int.self) ==
            expectedResult(useOrig, "public_enum_generic_func"))
}

DynamicallyReplaceable.test("DynamicallyReplaceable") {
  // First, test with only the original module.
	checkExpectedResults(forOriginalLibrary: true)

  // Now, test with the module containing the replacements.
	_ = dlopen("libModule2."+dylibSuffix, RTLD_NOW)
	checkExpectedResults(forOriginalLibrary: false)
}

runAllTests()
