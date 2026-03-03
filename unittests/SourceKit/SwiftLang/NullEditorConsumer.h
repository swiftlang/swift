//===----------------------------------------------------------------------===//
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

#include "SourceKit/Core/LangSupport.h"

namespace SourceKit {

class NullEditorConsumer : public EditorConsumer {
  bool needsSemanticInfo() override { return needsSema; }

  void handleRequestError(const char *Description) override {
    llvm_unreachable("unexpected error");
  }

  bool syntaxMapEnabled() override { return false; }

  void handleSyntaxMap(unsigned Offset, unsigned Length, UIdent Kind) override {
  }

  void handleSemanticAnnotation(unsigned Offset, unsigned Length, UIdent Kind,
                                bool isSystem) override {}

  void handleDeclaration(unsigned Offset, unsigned Length, UIdent Kind,
                         StringRef USR) override {}

  bool documentStructureEnabled() override { return false; }

  void beginDocumentSubStructure(
      unsigned Offset, unsigned Length, UIdent Kind, UIdent AccessLevel,
      UIdent SetterAccessLevel, unsigned NameOffset, unsigned NameLength,
      unsigned BodyOffset, unsigned BodyLength, unsigned DocOffset,
      unsigned DocLength, StringRef DisplayName, StringRef TypeName,
      StringRef RuntimeName, StringRef SelectorName,
      ArrayRef<StringRef> InheritedTypes,
      ArrayRef<std::tuple<UIdent, unsigned, unsigned>> Attrs) override {}

  void endDocumentSubStructure() override {}

  void handleDocumentSubStructureElement(UIdent Kind, unsigned Offset,
                                         unsigned Length) override {}

  void recordAffectedRange(unsigned Offset, unsigned Length) override {}

  void recordAffectedLineRange(unsigned Line, unsigned Length) override {}

  bool diagnosticsEnabled() override { return false; }

  void handleDiagnostics(ArrayRef<DiagnosticEntryInfo> DiagInfos,
                         UIdent DiagStage) override {}
  void recordFormattedText(StringRef Text) override {}

  void handleSourceText(StringRef Text) override {}

public:
  bool needsSema = false;
};

} // end namespace SourceKit
