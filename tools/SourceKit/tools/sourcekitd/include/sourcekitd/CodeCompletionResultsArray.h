#ifndef LLVM_SOURCEKITD_CODECOMPLETION_RESULTS_ARRAY_H
#define LLVM_SOURCEKITD_CODECOMPLETION_RESULTS_ARRAY_H

#include "sourcekitd/Internal.h"

namespace sourcekitd {

VariantFunctions *getVariantFunctionsForCodeCompletionResultsArray();

class CodeCompletionResultsArrayBuilder {
public:
  CodeCompletionResultsArrayBuilder();
  ~CodeCompletionResultsArrayBuilder();

  void add(SourceKit::UIdent Kind,
           llvm::StringRef Name,
           llvm::StringRef Description,
           llvm::StringRef SourceText,
           llvm::StringRef TypeName,
           Optional<llvm::StringRef> ModuleName,
           Optional<llvm::StringRef> DocBrief,
           Optional<llvm::StringRef> AssocUSRs,
           SourceKit::UIdent SemanticContext,
           bool NotRecommended,
           unsigned NumBytesToErase);

  std::unique_ptr<llvm::MemoryBuffer> createBuffer();

private:
  struct Implementation;
  Implementation &Impl;
};

}

#endif
