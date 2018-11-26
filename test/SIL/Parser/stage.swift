// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s -O -parse-stdlib -parse-as-library -emit-module -o %t/stage.swiftmodule
// RUN: %target-sil-opt %t/stage.swiftmodule -disable-sil-linking -sil-disable-ast-dump -o %t/stage.sil
// RUN: %target-sil-opt %t/stage.sil -o - | %FileCheck %s

// FIXME: We create all SIL modules in the 'raw' stage regardless of the input
// kind. If the primary input is a serialized module, we should assume the
// canonical stage. Also .sib files should have their stage
// serialized/deserialized.
//
// CHECK: sil_stage raw
//
// Verify that the '[canonical]' attribute was set.
// CHECK: sil [serialized] [canonical] @$s5stage21functionToReserializeyyF : $@convention(thin) () -> () {
@inlinable
public func functionToReserialize() {}
