// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-alias UncommonName=CommonName -typecheck -I %S/Inputs/custom-modules %s -Rmodule-loading 2> %t/load-result.output

// RUN: %FileCheck %s -input-file %t/load-result.output -check-prefix CHECK-FOO
// CHECK-FOO: import UncommonName
// CHECK-FOO-NEXT: remark: loaded module 'CommonName'

import UncommonName

_ = MyStruct()
_ = UncommonName.MyStruct()

// A -module-alias'd Clang module must be importable via canImport under both
// its alias and its real name.
#if !canImport(UncommonName)
#error("canImport(UncommonName) should succeed for a -module-alias'd Clang module")
#endif
#if !canImport(CommonName)
#error("canImport(CommonName) should still succeed")
#endif
