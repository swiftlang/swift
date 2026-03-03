// RUN: %target-swift-frontend -emit-ir -g %s | %FileCheck %s

public protocol P: ~Copyable {}

public typealias PP = P

// Make sure this doesn't crash:
public func withProto(_ e: borrowing any PP & ~Copyable) {}

// CHECK: !{{[0-9]+}} = !DICompositeType(tag: DW_TAG_structure_type, name: "$s4main1P_pRi_s_XPD", size: {{[0-9]+}}, runtimeLang: DW_LANG_Swift, identifier: "$s4main1P_pRi_s_XPD")
