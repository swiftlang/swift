// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-alias "//my/project:uncommon_name=CommonName" -typecheck -I %S/Inputs/custom-modules %s -Rmodule-loading 2> %t/load-result.output

// RUN: %FileCheck %s -input-file %t/load-result.output -check-prefix CHECK-FOO
// CHECK-FOO: import `//my/project:uncommon_name`
// CHECK-FOO-NEXT: remark: loaded module 'CommonName'

import `//my/project:uncommon_name`

_ = MyStruct()
_ = `//my/project:uncommon_name`.MyStruct()
