// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%{target-shared-library-prefix}Module1%{target-shared-library-suffix}) -DMODULE -module-name Module1 -emit-module -emit-module-path %t/Module1.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift-dylib(%t/%{target-shared-library-prefix}Module2%{target-shared-library-suffix}) -I%t -L%t -lModule1 -Xlinker -rpath -Xlinker %t -DMODULE2 -module-name Module2 -emit-module -emit-module-path %t/Module2.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift -I%t -L%t -lModule1 -DMAIN -o %t/main -Xlinker -rpath -Xlinker %t %s -swift-version 5
// RUN: %target-codesign %t/main %t/%target-library-name(Module1) %t/%target-library-name(Module2)
// RUN: %target-run %t/main %t/%target-library-name(Module1) %t/%target-library-name(Module2)

// Now the same in optimized mode.
// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%{target-shared-library-prefix}Module1%{target-shared-library-suffix}) -O -DMODULE -module-name Module1 -emit-module -emit-module-path %t/Module1.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift-dylib(%t/%{target-shared-library-prefix}Module2%{target-shared-library-suffix}) -O -I%t -L%t -lModule1 -Xlinker -rpath -Xlinker %t -DMODULE2 -module-name Module2 -emit-module -emit-module-path %t/Module2.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift -O -I%t -L%t -lModule1 -DMAIN -o %t/main -Xlinker -rpath -Xlinker %t %s -swift-version 5
// RUN: %target-codesign %t/main %t/%target-library-name(Module1) %t/%target-library-name(Module2)
// RUN: %target-run %t/main %t/%target-library-name(Module1) %t/%target-library-name(Module2)

// Now the same in size mode.
// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%{target-shared-library-prefix}Module1%{target-shared-library-suffix}) -Osize -DMODULE -module-name Module1 -emit-module -emit-module-path %t/Module1.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift-dylib(%t/%{target-shared-library-prefix}Module2%{target-shared-library-suffix}) -Osize -I%t -L%t -lModule1 -Xlinker -rpath -Xlinker %t -DMODULE2 -module-name Module2 -emit-module -emit-module-path %t/Module2.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift -Osize -I%t -L%t -lModule1 -DMAIN -o %t/main -Xlinker -rpath -Xlinker %t %s -swift-version 5
// RUN: %target-codesign %t/main %t/%target-library-name(Module1) %t/%target-library-name(Module2)
// RUN: %target-run %t/main %t/%target-library-name(Module1) %t/%target-library-name(Module2)

// Now the same in optimized wholemodule mode.
// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%{target-shared-library-prefix}Module1%{target-shared-library-suffix}) -O -wmo -DMODULE -module-name Module1 -emit-module -emit-module-path %t/Module1.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift-dylib(%t/%{target-shared-library-prefix}Module2%{target-shared-library-suffix}) -O -wmo -I%t -L%t -lModule1 -Xlinker -rpath -Xlinker %t -DMODULE2 -module-name Module2 -emit-module -emit-module-path %t/Module2.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift -O -wmo -I%t -L%t -lModule1 -DMAIN -o %t/main -Xlinker -rpath -Xlinker %t %s -swift-version 5
// RUN: %target-codesign %t/main %t/%target-library-name(Module1) %t/%target-library-name(Module2)
// RUN: %target-run %t/main %t/%target-library-name(Module1) %t/%target-library-name(Module2)

// Test the -enable-implicit-dynamic flag.

// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%{target-shared-library-prefix}Module1%{target-shared-library-suffix}) -DMODULENODYNAMIC -module-name Module1 -emit-module -emit-module-path %t/Module1.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift -Xfrontend -enable-implicit-dynamic
// RUN: %target-build-swift-dylib(%t/%{target-shared-library-prefix}Module2%{target-shared-library-suffix}) -I%t -L%t -lModule1 -Xlinker -rpath -Xlinker %t -DMODULE2 -module-name Module2 -emit-module -emit-module-path %t/Module2.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift -I%t -L%t -lModule1 -DMAIN -o %t/main -Xlinker -rpath -Xlinker %t %s -swift-version 5
// RUN: %target-codesign %t/main %t/%target-library-name(Module1) %t/%target-library-name(Module2)
// RUN: %target-run %t/main %t/%target-library-name(Module1) %t/%target-library-name(Module2)

// Test the -enable-implicit-dynamic flag in optimized wholemodule mode.
// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%{target-shared-library-prefix}Module1%{target-shared-library-suffix}) -O -wmo -DMODULENODYNAMIC -module-name Module1 -emit-module -emit-module-path %t/Module1.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift -Xfrontend -enable-implicit-dynamic
// RUN: %target-build-swift-dylib(%t/%{target-shared-library-prefix}Module2%{target-shared-library-suffix}) -O -wmo -I%t -L%t -lModule1 -Xlinker -rpath -Xlinker %t -DMODULE2 -module-name Module2 -emit-module -emit-module-path %t/Module2.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_module.swift
// RUN: %target-build-swift -O -wmo -I%t -L%t -lModule1 -DMAIN -o %t/main -Xlinker -rpath -Xlinker %t %s -swift-version 5
// RUN: %target-codesign %t/main %t/%target-library-name(Module1) %t/%target-library-name(Module2)
// RUN: %target-run %t/main %t/%target-library-name(Module1) %t/%target-library-name(Module2)


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
 expectTrue(public_global_var == expectedResult(useOrig, "public_global_var"))

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
  var executablePath = CommandLine.arguments[0]
  executablePath.removeLast(4)

  // First, test with only the original module.
	checkExpectedResults(forOriginalLibrary: true)

  // Now, test with the module containing the replacements.

#if os(Linux)
	_ = dlopen("libModule2."+dylibSuffix, RTLD_NOW)
#else
	_ = dlopen(executablePath+"libModule2."+dylibSuffix, RTLD_NOW)
#endif
	checkExpectedResults(forOriginalLibrary: false)
}

runAllTests()
