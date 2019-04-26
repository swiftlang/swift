// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -emit-module %s -module-name HasArray -o %t -module-cache-path %t/mcp -group-info-path %t/nonexist.json -emit-module-doc &> %t/result.txt
// RUN: %FileCheck %s -check-prefix=MISSING < %t/result.txt
// RUN: not %target-swift-frontend -emit-module %s -module-name HasArray -o %t -module-cache-path %t/mcp -group-info-path %S/Inputs/corrupted_group_info.json -emit-module-doc &> %t/result.txt
// RUN: %FileCheck %s -check-prefix=CORRUPTED < %t/result.txt

// RUN: %target-swift-frontend -emit-module %s -module-name HasArray -o %t -module-cache-path %t/mcp -group-info-path %S/Inputs/group_info.json -emit-module-doc &> %t/result.txt
// RUN: %llvm-strings %t/HasArray.swiftdoc > %t/doc_strings.txt
// RUN: %FileCheck %s -check-prefix=INCLUDED < %t/doc_strings.txt
// RUN: %FileCheck %s -check-prefix=EXCLUDED < %t/doc_strings.txt

public protocol PublicProtocol {}

internal class InternalClass {}

// MISSING: error: cannot find group info file at path
// CORRUPTED: error: cannot parse group info file at path
// EXCLUDED-NOT: InternalClass
// INCLUDED: PublicProtocol
