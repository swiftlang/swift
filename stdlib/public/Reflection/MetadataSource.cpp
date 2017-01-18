//===--- MetadataSource.cpp - Swift Metadata Sources for Reflection -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Reflection/MetadataSource.h"

#include <sstream>

using namespace swift;
using namespace reflection;

class PrintMetadataSource
  : public MetadataSourceVisitor<PrintMetadataSource, void> {
  std::ostream &OS;
  unsigned Indent;

  std::ostream &indent(unsigned Amount) {
    for (unsigned i = 0; i < Amount; ++i)
      OS << ' ';
    return OS;
  }

  std::ostream &printHeader(std::string Name) {
    indent(Indent) << '(' << Name;
    return OS;
  }

  template<typename T>
  std::ostream &printField(std::string name, const T &value) {
    if (!name.empty())
      OS << " " << name << "=" << value;
    else
      OS << " " << value;
    return OS;
  }

  void printRec(const MetadataSource *MS) {
    OS << "\n";

    Indent += 2;
    visit(MS);
    Indent -= 2;
  }

  void closeForm() {
    OS << ')';
  }

public:
  PrintMetadataSource(std::ostream &OS, unsigned Indent)
    : OS(OS), Indent(Indent) {}

  void
  visitClosureBindingMetadataSource(const ClosureBindingMetadataSource *CB) {
    printHeader("closure_binding");
    printField("index", CB->getIndex());
    closeForm();
  }

  void
  visitReferenceCaptureMetadataSource(const ReferenceCaptureMetadataSource *RC){
    printHeader("reference_capture");
    printField("index", RC->getIndex());
    closeForm();
  }

  void
  visitMetadataCaptureMetadataSource(const MetadataCaptureMetadataSource *MC){
    printHeader("metadata_capture");
    printField("index", MC->getIndex());
    closeForm();
  }

  void
  visitGenericArgumentMetadataSource(const GenericArgumentMetadataSource *GA) {
    printHeader("generic_argument");
    printField("index", GA->getIndex());
    printRec(GA->getSource());
    closeForm();
  }

  void
  visitParentMetadataSource(const ParentMetadataSource *P) {
    printHeader("parent_of");
    printRec(P->getChild());
    closeForm();
  }

  void visitSelfMetadataSource(const SelfMetadataSource *S) {
    printHeader("self");
    closeForm();
  }

  void
  visitSelfWitnessTableMetadataSource(const SelfWitnessTableMetadataSource *W) {
    printHeader("self_witness_table");
    closeForm();
  }
};

void MetadataSource::dump() const {
  dump(std::cerr, 0);
}

void MetadataSource::dump(std::ostream &OS, unsigned Indent) const {
  PrintMetadataSource(OS, Indent).visit(this);
  OS << '\n';
}
