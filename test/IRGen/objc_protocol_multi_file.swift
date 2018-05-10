// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s %S/Inputs/objc_protocol_multi_file_helper.swift -g -emit-ir | %FileCheck %s

// This used to crash <rdar://problem/17929944>.
// To tickle the crash, SubProto must not be used elsewhere in this file.
protocol SubProto : BaseProto {}
// CHECK: @"$S24objc_protocol_multi_file8SubProtoMp" = hidden constant %swift.protocol

protocol DoubleSubProto : IntermediateProto {}
// CHECK: @"$S24objc_protocol_multi_file14DoubleSubProtoMp" = hidden constant %swift.protocol
