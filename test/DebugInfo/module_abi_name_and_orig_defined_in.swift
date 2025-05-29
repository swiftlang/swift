// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -module-name=Hello -module-abi-name Goodbye -emit-ir -o - | %FileCheck %s

// REQUIRES: OS=macosx
//
@available(iOS 1.0, macOS 1.0, tvOS 1.0, watchOS 1.0, *)
@_originallyDefinedIn(
     module: "ThirdModule", iOS 2.0, macOS 2.0, tvOS 2.0, watchOS 2.0)
public class DefinedElsewhere {}
// CHECK: DICompositeType(tag: DW_TAG_structure_type, name: "DefinedElsewhere",{{.*}}runtimeLang: DW_LANG_Swift, identifier: "$s11ThirdModule16DefinedElsewhereCD")

let v2 = DefinedElsewhere()
