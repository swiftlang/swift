#ifndef LLVM_SOURCEKIT_LIB_SWIFTLANG_SWIFTINVOCATION_H
#define LLVM_SOURCEKIT_LIB_SWIFTLANG_SWIFTINVOCATION_H

#include "swift/Basic/ThreadSafeRefCounted.h"
#include <vector>

namespace swift {
  class CompilerInvocation;
}

namespace SourceKit {
  class SwiftASTManager;

/// Encompasses an invocation for getting an AST. This is used to control AST
/// sharing among different requests.
class SwiftInvocation : public swift::ThreadSafeRefCountedBase<SwiftInvocation> {
public:
  ~SwiftInvocation();

  struct Implementation;
  Implementation &Impl;

  void applyTo(swift::CompilerInvocation &CompInvok) const;
  void raw(std::vector<std::string> &Args, std::string &PrimaryFile) const;

private:
  SwiftInvocation(Implementation &Impl) : Impl(Impl) { }
  friend class SwiftASTManager;
};

} // namespace SourceKit.

#endif
