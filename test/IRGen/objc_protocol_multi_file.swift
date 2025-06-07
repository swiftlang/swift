// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -enable-objc-interop -primary-file %s %S/Inputs/objc_protocol_multi_file_helper.swift -g -emit-ir | %FileCheck %s
// REQUIRES: objc_codegen

// This used to crash <rdar://problem/17929944>.
// To tickle the crash, SubProto must not be used elsewhere in this file.
protocol SubProto : BaseProto {}
// CHECK: @"$s24objc_protocol_multi_file8SubProtoMp" = hidden constant 

protocol DoubleSubProto : IntermediateProto {}
// CHECK: @"$s24objc_protocol_multi_file14DoubleSubProtoMp" = hidden constant
