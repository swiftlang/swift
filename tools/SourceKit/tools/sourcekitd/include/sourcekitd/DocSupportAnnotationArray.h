#ifndef LLVM_SOURCEKITD_DOCSUPPORT_ANNOTATION_ARRAY_H
#define LLVM_SOURCEKITD_DOCSUPPORT_ANNOTATION_ARRAY_H

#include "sourcekitd/Internal.h"
#include "llvm/ADT/SmallString.h"

namespace SourceKit {
  struct DocEntityInfo;
}

namespace sourcekitd {

VariantFunctions *getVariantFunctionsForDocSupportAnnotationArray();

class DocSupportAnnotationArrayBuilder {
public:
  DocSupportAnnotationArrayBuilder();
  ~DocSupportAnnotationArrayBuilder();

  void add(const SourceKit::DocEntityInfo &Info);
  std::unique_ptr<llvm::MemoryBuffer> createBuffer();

private:
  struct Implementation;
  Implementation &Impl;
};

}

#endif
