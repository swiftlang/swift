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

#if SWIFT_ENABLE_REFLECTION

#include "swift/Reflection/MetadataSource.h"
#include <iostream>

using namespace swift;
using namespace reflection;

class PrintMetadataSource
  : public MetadataSourceVisitor<PrintMetadataSource, void> {
  std::ostream &stream;
  unsigned Indent;

  std::ostream &indent(unsigned Amount) {
    for (unsigned i = 0; i < Amount; ++i)
      stream << " ";
    return stream;
  }

  std::ostream &printHeader(std::string Name) {
    indent(Indent) << "(" << Name;
    return stream;
  }

  std::ostream &printField(std::string name, std::string value) {
    if (!name.empty())
      stream << " " << name << "=" << value;
    else
      stream << " " << value;
    return stream;
  }

  void printRec(const MetadataSource *MS) {
    stream << "\n";

    Indent += 2;
    visit(MS);
    Indent -= 2;
  }

  void closeForm() {
    stream << ")";
  }

public:
  PrintMetadataSource(std::ostream &stream, unsigned Indent)
      : stream(stream), Indent(Indent) {}

  void
  visitClosureBindingMetadataSource(const ClosureBindingMetadataSource *CB) {
    printHeader("closure_binding");
    printField("index", std::to_string(CB->getIndex()));
    closeForm();
  }

  void
  visitReferenceCaptureMetadataSource(const ReferenceCaptureMetadataSource *RC){
    printHeader("reference_capture");
    printField("index", std::to_string(RC->getIndex()));
    closeForm();
  }

  void
  visitMetadataCaptureMetadataSource(const MetadataCaptureMetadataSource *MC){
    printHeader("metadata_capture");
    printField("index", std::to_string(MC->getIndex()));
    closeForm();
  }

  void
  visitGenericArgumentMetadataSource(const GenericArgumentMetadataSource *GA) {
    printHeader("generic_argument");
    printField("index", std::to_string(GA->getIndex()));
    printRec(GA->getSource());
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

void MetadataSource::dump() const { dump(std::cerr, 0); }

void MetadataSource::dump(std::ostream &stream, unsigned Indent) const {
  PrintMetadataSource(stream, Indent).visit(this);
  stream << "\n";
}

#endif
