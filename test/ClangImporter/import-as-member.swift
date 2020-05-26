// RUN: %empty-directory(%t.mcp)
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -F %S/Inputs/frameworks -I %S/Inputs/custom-modules -module-cache-path %t.mcp %s 2>&1 | %FileCheck %S/Inputs/custom-modules/ImportAsMember.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -F %S/Inputs/frameworks -I %S/Inputs/custom-modules -module-cache-path %t.mcp %s -verify

import ImportAsMember
import ImportAsMemberSubmodules

let _: IAMSOuter.Inner?
let _: IAMMultipleNested.Inner? // expected-error {{ambiguous type name 'Inner' in 'IAMMultipleNested'}}

func testCreateShadowing(d: Double) -> Struct1 {
  return Struct1(x: d, y: d, z: d)
}
