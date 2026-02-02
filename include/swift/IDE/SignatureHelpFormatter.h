//===--- SignatureHelpFormatter.h --- ---------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_SIGNATURE_HELP_FORMATTER_H
#define SWIFT_IDE_SIGNATURE_HELP_FORMATTER_H

#include "swift/IDE/SignatureHelp.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Allocator.h"

namespace swift {

class DeclContext;

namespace ide {

class CodeCompletionString;

struct FormattedSignatureHelp {
  struct Parameter {
    /// The offset of the parameter text in the signature text.
    unsigned Offset;

    /// The length of the parameter text in the signature text.
    unsigned Length;

    /// The internal parameter name.
    llvm::StringRef Name;

    Parameter() {}
  };

  struct Signature {
    llvm::StringRef Text;
    llvm::StringRef DocComment;
    std::optional<unsigned> ActiveParam;
    llvm::ArrayRef<Parameter> Params;

    Signature(llvm::StringRef Text, llvm::StringRef DocComment,
              std::optional<unsigned> ActiveParam,
              llvm::ArrayRef<Parameter> Params)
        : Text(Text), DocComment(DocComment), ActiveParam(ActiveParam),
          Params(Params) {}
  };

  llvm::ArrayRef<Signature> Signatures;
  unsigned ActiveSignature;

  FormattedSignatureHelp(llvm::ArrayRef<Signature> Signatures,
                         unsigned ActiveSignature)
      : Signatures(Signatures), ActiveSignature(ActiveSignature) {}
};

class SignatureHelpFormatter {
private:
  llvm::BumpPtrAllocator &Allocator;

public:
  SignatureHelpFormatter(llvm::BumpPtrAllocator &Allocator)
      : Allocator(Allocator) {}

  FormattedSignatureHelp format(ide::SignatureHelpResult Result);

private:
  FormattedSignatureHelp::Signature
  formatSignature(const DeclContext *DC, const ide::Signature &Signature);

  CodeCompletionString *createSignatureString(const ide::Signature &Signature,
                                              const DeclContext *DC);
};

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_SIGNATURE_HELP_FORMATTER_H
