//===---- FineGrainedDependencyFormat.h - swiftdeps format ---*- C++ -*-======//
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

#ifndef SWIFT_AST_FINEGRAINEDDEPENDENCYFORMAT_H
#define SWIFT_AST_FINEGRAINEDDEPENDENCYFORMAT_H

#include "llvm/Bitcode/RecordLayout.h"
#include "llvm/Bitstream/BitCodes.h"

namespace llvm {
class MemoryBuffer;
}

namespace swift {

class DiagnosticEngine;

namespace fine_grained_dependencies {

class SourceFileDepGraph;

using llvm::BCFixed;
using llvm::BCVBR;
using llvm::BCBlob;
using llvm::BCRecordLayout;

const unsigned char FINE_GRAINED_DEPDENENCY_FORMAT_SIGNATURE[] = {'D', 'E', 'P', 'S'};

const unsigned FINE_GRAINED_DEPENDENCY_FORMAT_VERSION_MAJOR = 1;

/// Increment this on every change.
const unsigned FINE_GRAINED_DEPENDENCY_FORMAT_VERSION_MINOR = 0;

using IdentifierIDField = BCVBR<13>;

using NodeKindField = BCFixed<3>;
using DeclAspectField = BCFixed<1>;

const unsigned RECORD_BLOCK_ID = llvm::bitc::FIRST_APPLICATION_BLOCKID;

namespace record_block {
  enum {
    METADATA = 1,
    SOURCE_FILE_DEP_GRAPH_NODE,
    FINGERPRINT_NODE,
    DEPENDS_ON_DEFINITION_NODE,
    IDENTIFIER_NODE,
  };

  using MetadataLayout = BCRecordLayout<
    METADATA, // ID
    BCFixed<16>, // Dependency graph format major version
    BCFixed<16>, // Dependency graph format minor version
    BCBlob // Compiler version string
  >;

  using SourceFileDepGraphNodeLayout = BCRecordLayout<
    SOURCE_FILE_DEP_GRAPH_NODE, // ID
    NodeKindField, // Dependency key node kind
    DeclAspectField, // Dependency key declaration aspect
    IdentifierIDField, // Dependency key mangled context type name
    IdentifierIDField, // Dependency key basic name
    BCFixed<1> // Is this a "provides" node?
  >;

  // Optionally follows DEPENDS_ON_DEFINITION_NODE.
  using FingerprintNodeLayout = BCRecordLayout<
    FINGERPRINT_NODE,
    BCBlob
  >;

  // Optionally follows SOURCE_FILE_DEP_GRAPH_NODE and FINGERPRINT_NODE.
  using DependsOnDefNodeLayout = BCRecordLayout<
    DEPENDS_ON_DEFINITION_NODE,
    BCVBR<16>
  >;

  // Optionally follows all other nodes.
  using IdentifierNodeLayout = BCRecordLayout<
    IDENTIFIER_NODE,
    BCBlob
  >;
}

bool readFineGrainedDependencyGraph(llvm::MemoryBuffer &buffer,
                                    SourceFileDepGraph &g);

bool readFineGrainedDependencyGraph(llvm::StringRef path,
                                    SourceFileDepGraph &g);

bool writeFineGrainedDependencyGraph(DiagnosticEngine &diags, llvm::StringRef path,
                                     const SourceFileDepGraph &g);

} // namespace fine_grained_dependencies
} // namespace swift

#endif
