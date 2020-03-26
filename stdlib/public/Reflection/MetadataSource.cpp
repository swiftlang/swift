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
  FILE *file;
  unsigned Indent;

  FILE * indent(unsigned Amount) {
    for (unsigned i = 0; i < Amount; ++i)
      fprintf(file, " ");
    return file;
  }

  FILE * printHeader(std::string Name) {
    fprintf(indent(Indent), "(%s", Name.c_str());
    return file;
  }

  FILE * printField(std::string name, std::string value) {
    if (!name.empty())
      fprintf(file, " %s=%s", name.c_str(), value.c_str());
    else
      fprintf(file, " %s", value.c_str());
    return file;
  }

  void printRec(const MetadataSource *MS) {
    fprintf(file, "\n");

    Indent += 2;
    visit(MS);
    Indent -= 2;
  }

  void closeForm() {
    fprintf(file, ")");
  }

public:
  PrintMetadataSource(FILE *file, unsigned Indent)
    : file(file), Indent(Indent) {}

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

void MetadataSource::dump() const {
  dump(stderr, 0);
}

void MetadataSource::dump(FILE *file, unsigned Indent) const {
  PrintMetadataSource(file, Indent).visit(this);
  fprintf(file, "\n");
}
