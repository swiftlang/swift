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

#include "llvm/Bitcode/BitcodeConvenience.h"
#include "llvm/Bitstream/BitCodes.h"

namespace llvm {
class MemoryBuffer;
namespace vfs {
class OutputBackend;
}
}

namespace swift {

class DiagnosticEngine;

namespace fine_grained_dependencies {

class SourceFileDepGraph;

using llvm::BCFixed;
using llvm::BCVBR;
using llvm::BCBlob;
using llvm::BCRecordLayout;

/// Every .swiftdeps file begins with these 4 bytes, for easy identification when
/// debugging.
const unsigned char FINE_GRAINED_DEPENDENCY_FORMAT_SIGNATURE[] = {'D', 'E', 'P', 'S'};

const unsigned FINE_GRAINED_DEPENDENCY_FORMAT_VERSION_MAJOR = 1;

/// Increment this on every change.
const unsigned FINE_GRAINED_DEPENDENCY_FORMAT_VERSION_MINOR = 0;

using IdentifierIDField = BCVBR<13>;

using NodeKindField = BCFixed<3>;
using DeclAspectField = BCFixed<1>;

const unsigned RECORD_BLOCK_ID = llvm::bitc::FIRST_APPLICATION_BLOCKID;
const unsigned INCREMENTAL_INFORMATION_BLOCK_ID = 196;

/// The swiftdeps file format consists of a METADATA record, followed by zero or more
/// IDENTIFIER_NODE records.
///
/// Then, there is one SOURCE_FILE_DEP_GRAPH_NODE for each serialized
/// SourceFileDepGraphNode. These are followed by FINGERPRINT_NODE and
/// DEPENDS_ON_DEFINITION_NODE, if the node has a fingerprint and depends-on
/// definitions, respectively.
namespace record_block {
  enum {
    METADATA = 1,
    SOURCE_FILE_DEP_GRAPH_NODE,
    FINGERPRINT_NODE,
    DEPENDS_ON_DEFINITION_NODE,
    IDENTIFIER_NODE,
  };

  // Always the first record in the file.
  using MetadataLayout = BCRecordLayout<
    METADATA, // ID
    BCFixed<16>, // Dependency graph format major version
    BCFixed<16>, // Dependency graph format minor version
    BCBlob // Compiler version string
  >;

  // After the metadata record, we have zero or more identifier records,
  // for each unique string that is referenced from a SourceFileDepGraphNode.
  //
  // Identifiers are referenced by their sequence number, starting from 1.
  // The identifier value 0 is special; it always represents the empty string.
  // There is no IDENTIFIER_NODE serialized that corresponds to it, instead
  // the first IDENTIFIER_NODE always has a sequence number of 1.
  using IdentifierNodeLayout = BCRecordLayout<
    IDENTIFIER_NODE,
    BCBlob
  >;

  using SourceFileDepGraphNodeLayout = BCRecordLayout<
    SOURCE_FILE_DEP_GRAPH_NODE, // ID
    // The next four fields correspond to the fields of the DependencyKey
    // structure.
    NodeKindField, // DependencyKey::kind
    DeclAspectField, // DependencyKey::aspect
    IdentifierIDField, // DependencyKey::context
    IdentifierIDField, // DependencyKey::name
    BCFixed<1> // Is this a "provides" node?
  >;

  // Follows DEPENDS_ON_DEFINITION_NODE when the SourceFileDepGraphNode has a
  // fingerprint set.
  using FingerprintNodeLayout = BCRecordLayout<
    FINGERPRINT_NODE,
    BCBlob
  >;

  // Follows SOURCE_FILE_DEP_GRAPH_NODE and FINGERPRINT_NODE when the
  // SourceFileDepGraphNode has one or more depends-on entries.
  using DependsOnDefNodeLayout = BCRecordLayout<
    DEPENDS_ON_DEFINITION_NODE,
    BCVBR<16> // The sequence number (starting from 0) of the referenced
              // SOURCE_FILE_DEP_GRAPH_NODE
  >;
}

/// Tries to read the dependency graph from the given buffer.
/// Returns \c true if there was an error.
bool readFineGrainedDependencyGraph(llvm::MemoryBuffer &buffer,
                                    SourceFileDepGraph &g);

/// Tries to read the dependency graph from the given buffer, assuming that it
/// is in the format of a swiftmodule file.
/// Returns \c true if there was an error.
bool readFineGrainedDependencyGraphFromSwiftModule(llvm::MemoryBuffer &buffer,
                                                   SourceFileDepGraph &g);

/// Tries to read the dependency graph from the given path name.
/// Returns true if there was an error.
bool readFineGrainedDependencyGraph(llvm::StringRef path,
                                    SourceFileDepGraph &g);

/// Tries to write the dependency graph to the given path name.
/// Returns true if there was an error.
bool writeFineGrainedDependencyGraphToPath(DiagnosticEngine &diags,
                                           llvm::vfs::OutputBackend &backend,
                                           llvm::StringRef path,
                                           const SourceFileDepGraph &g);

/// Enumerates the supported set of purposes for writing out or reading in
/// swift dependency information into a file. These can be used to influence
/// the structure of the resulting data that is produced by the serialization
/// machinery defined here.
enum class Purpose : bool {
  /// Write out fine grained dependency metadata suitable for embedding in
  /// \c .swiftmodule file.
  ///
  /// The resulting metadata does not contain the usual block descriptor header
  /// nor does it contain a leading magic signature, which would otherwise
  /// disrupt clients and tools that do not expect them to be present such as
  /// llvm-bcanalyzer.
  ForSwiftModule = false,
  /// Write out fine grained dependency metadata suitable for a standalone
  /// \c .swiftdeps file.
  ///
  /// The resulting metadata will contain a leading magic signature and block
  /// descriptor header.
  ForSwiftDeps = true,
};

/// Tries to write out the given dependency graph with the given
/// bitstream writer.
void writeFineGrainedDependencyGraph(llvm::BitstreamWriter &Out,
                                     const SourceFileDepGraph &g,
                                     Purpose purpose);

} // namespace fine_grained_dependencies
} // namespace swift

#endif
