#include "sourcekitd/DocSupportAnnotationArray.h"
#include "sourcekitd/CompactArray.h"
#include "SourceKit/Core/LangSupport.h"
#include "llvm/Support/MemoryBuffer.h"

#include "DictionaryKeys.h"

using namespace SourceKit;
using namespace sourcekitd;

struct DocSupportAnnotationArrayBuilder::Implementation {
  CompactArrayBuilder<UIdent,
                      Optional<StringRef>,
                      Optional<StringRef>,
                      unsigned,
                      unsigned> Builder;
};

DocSupportAnnotationArrayBuilder::DocSupportAnnotationArrayBuilder()
  : Impl(*new Implementation()) {

}

DocSupportAnnotationArrayBuilder::~DocSupportAnnotationArrayBuilder() {
  delete &Impl;
}

void DocSupportAnnotationArrayBuilder::add(const DocEntityInfo &Info) {
  Optional<StringRef> NameOpt;
  if (!Info.Name.empty())
    NameOpt = Info.Name;
  Optional<StringRef> USROpt;
  if (!Info.USR.empty())
    USROpt = Info.USR;
  Impl.Builder.addEntry(Info.Kind,
                        NameOpt,
                        USROpt,
                        Info.Offset,
                        Info.Length);
}

std::unique_ptr<llvm::MemoryBuffer>
DocSupportAnnotationArrayBuilder::createBuffer() {
  return Impl.Builder.createBuffer();
}

namespace {

class DocSupportAnnotationArray {
public:
  typedef CompactArrayReader<sourcekitd_uid_t,
                             const char *,
                             const char *,
                             unsigned,
                             unsigned> CompactArrayReaderTy;
  
  static bool dictionary_apply(void *Buf, size_t Index,
                              sourcekitd_variant_dictionary_applier_t applier) {
    CompactArrayReaderTy Reader(Buf);

    sourcekitd_uid_t Kind;
    const char *Name;
    const char *USR;
    unsigned Offset;
    unsigned Length;

    Reader.readEntries(Index,
                  Kind,
                  Name,
                  USR,
                  Offset,
                  Length);

#define APPLY(K, Ty, Field)                              \
  do {                                                   \
    sourcekitd_uid_t key = SKDUIDFromUIdent(K);          \
    sourcekitd_variant_t var = make##Ty##Variant(Field); \
    if (!applier(key, var)) return false;                \
  } while (0)

    APPLY(KeyKind, UID, Kind);
    if (Name) {
      APPLY(KeyName, String, Name);
    }
    if (USR) {
      APPLY(KeyUSR, String, USR);
    }
    APPLY(KeyOffset, Int, Offset);
    APPLY(KeyLength, Int, Length);

    return true;
  }
};

}

VariantFunctions *
sourcekitd::getVariantFunctionsForDocSupportAnnotationArray() {
  return &CompactArrayFuncs<DocSupportAnnotationArray>::Funcs;
}
