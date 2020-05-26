// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mock-sdk)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/PrebuiltModuleCache)

// This test makes sure that we propagate the right set of dependencies from a
// prebuilt module to its corresponding forwarding module.

// Setup. Copy the mock SDK into the tmp directory.
// RUN: cp -r %S/Inputs/mock-sdk/* %t/mock-sdk/.

// 1. Compile ExportedLib.swiftinterface, which a) is in the SDK, and b) depends
//    on a C module with headers that should be in the dependency list.
//    Put it in the prebuilt cache.

// RUN: %target-swift-frontend -compile-module-from-interface %t/mock-sdk/ExportedLib.swiftinterface -sdk %t/mock-sdk -o %t/PrebuiltModuleCache/ExportedLib.swiftmodule -serialize-parseable-module-interface-dependency-hashes -track-system-dependencies

// 2. Make sure the prebuilt module we built has SomeCModule.h as a dependency.

// RUN: llvm-bcanalyzer -dump %t/PrebuiltModuleCache/ExportedLib.swiftmodule | grep 'FILE_DEPENDENCY.*SomeCModule.h'

// 3. Typecheck this file, which imports SdkLib, which imports ExportedLib.
//    Because ExportedLib is prebuilt, we expect a forwarding module for
//    ExportedLib in the module cache, and a serialized module for SdkLib in
//    the cache.

// RUN: %target-swift-frontend -typecheck %s -prebuilt-module-cache-path %t/PrebuiltModuleCache -module-cache-path %t/ModuleCache -sdk %t/mock-sdk -I %t/mock-sdk -track-system-dependencies

// 4. Make sure the forwarding module is installed in the cache.

// RUN: %{python} %S/Inputs/check-is-forwarding-module.py %t/ModuleCache/ExportedLib-*.swiftmodule

// 5. Make sure the forwarding module depends on the prebuilt module and the C
//    header.

// RUN: grep '  *path:.*ExportedLib.swiftmodule' %t/ModuleCache/ExportedLib-*.swiftmodule
// RUN: grep '  *path:.*SomeCModule.h' %t/ModuleCache/ExportedLib-*.swiftmodule

// 6. Make sure the dependencies from the forwarding module make it into the
//    cached module.

// RUN: llvm-bcanalyzer -dump %t/ModuleCache/SdkLib-*.swiftmodule | grep 'FILE_DEPENDENCY.*SomeCModule.h'

// 7. Make sure the prebuilt ExportedLib module did NOT get propagated to SdkLib.

// RUN: llvm-bcanalyzer -dump %t/ModuleCache/SdkLib-*.swiftmodule | not grep 'FILE_DEPENDENCY.*PrebuiltModuleCache'

// 8. Make sure we re-build the SdkLib module if one of the dependencies changes its mtime (but not its hash).
//    This will ensure we recorded the forwarding module's dependencies, not the prebuilt.

// RUN: %{python} %S/Inputs/make-old.py %t/ModuleCache/SdkLib-*.swiftmodule
// RUN: %{python} %S/Inputs/make-old.py %t/mock-sdk/usr/include/SomeCModule.h
// RUN: %target-swift-frontend -typecheck %s -prebuilt-module-cache-path %t/PrebuiltModuleCache -module-cache-path %t/ModuleCache -sdk %t/mock-sdk -I %t/mock-sdk -track-system-dependencies
// RUN: %{python} %S/Inputs/check-is-new.py %t/ModuleCache/SdkLib-*.swiftmodule

import SdkLib