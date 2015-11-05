#ifndef LLVM_SOURCEKIT_SUPPORT_UIDENT_H
#define LLVM_SOURCEKIT_SUPPORT_UIDENT_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Compiler.h"

namespace llvm {
  class raw_ostream;
}

namespace SourceKit {

/// \brief A string identifier that is uniqued for the process lifetime.
/// The string identifier should not contain any spaces.
class UIdent {
  void *Ptr = 0;

  explicit UIdent(void *Ptr) : Ptr(Ptr) { }

public:
  UIdent() = default;
  explicit UIdent(llvm::StringRef Str);

  bool isValid() const { return Ptr != 0; }
  bool isInvalid() const { return !isValid(); }

  void *getAsOpaqueValue() const { return Ptr; }
  static UIdent getFromOpaqueValue(void *Val) {
    return UIdent(Val);
  }

  void setTag(void *Tag);
  void *getTag() const;

  friend bool operator==(const UIdent &LHS, const UIdent &RHS) {
    return LHS.Ptr == RHS.Ptr;
  }
  friend bool operator!=(const UIdent &LHS, const UIdent &RHS) {
    return !(LHS == RHS);
  }

  llvm::StringRef getName() const;
  const char *c_str() const;

  LLVM_ATTRIBUTE_USED void dump() const;
  void print(llvm::raw_ostream &OS) const;
};

class LazyUIdent {
  const char *Name;
  mutable std::atomic<UIdent> UID;
public:
  LazyUIdent(const char *Name) : Name(Name) { }

  UIdent get() const {
    if (UID.load().isInvalid())
      UID = UIdent(Name);
    return UID;
  }

  operator UIdent() const {
    return get();
  }
};

} // namespace SourceKit

#endif
