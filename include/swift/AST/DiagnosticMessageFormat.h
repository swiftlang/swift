//===--- DiagnosticMessageFormat.h - YAML format for Diagnostic Messages ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the YAML format for diagnostic messages.
// 
//===----------------------------------------------------------------------===//

#include "llvm/Support/YAMLTraits.h"
#include "DiagnosticList.cpp"

namespace llvm {
namespace yaml {

template<>
struct ScalarEnumerationTraits<swift::DiagID> {
  static void enumeration(IO &io, swift::DiagID &value) {
#define DIAG(KIND,ID,Options,Text,Signature) io.enumCase(value, #ID, swift::DiagID::ID);
#include "swift/AST/DiagnosticsAll.def"
  }
};

template <>
struct MappingTraits<swift::DiagnosticNode> {
  static void mapping(IO &io, swift::DiagnosticNode &node) {
    io.mapRequired("id", node.id);
    io.mapRequired("msg", node.msg);
  }
};

}
}

LLVM_YAML_IS_SEQUENCE_VECTOR(swift::DiagnosticNode)
