inline int &copyCount() {
  static int count = 0;
  return count;
}

struct CopyCounter {
  CopyCounter() {}
  CopyCounter(const CopyCounter &);
  CopyCounter(CopyCounter &&) {}
};

// Defined out of line, so it is only emitted if IRGen finds a use of it.
inline CopyCounter::CopyCounter(const CopyCounter &) { copyCount()++; }

// A minimal type-erased callable, similar to std::function. Only the virtual
// clone() copies the stored callable.
struct ErasedBase {
  virtual ~ErasedBase() {}
  virtual ErasedBase *clone() const = 0;
};

template <class F>
struct ErasedImpl : ErasedBase {
  F function;
  ErasedImpl(F &&function) : function(static_cast<F &&>(function)) {}
  ErasedImpl(const F &function) : function(function) {}
  ErasedBase *clone() const override { return new ErasedImpl(function); }
};

struct Erased {
  ErasedBase *impl;
  template <class F>
  Erased(F function) : impl(new ErasedImpl<F>(static_cast<F &&>(function))) {}
  Erased(const Erased &other) : impl(other.impl->clone()) {}
  ~Erased() { delete impl; }
};

// Swift instantiates copyLambda(), so IRGen has to find what the lambda's
// implicit copy constructor calls. Clang only defines that copy constructor
// when IRGen instantiates ErasedImpl::clone().
template <class T>
struct LambdaCopier {
  int copyLambda() const {
    CopyCounter counter;
    Erased erased([counter = static_cast<CopyCounter &&>(counter)]() {});
    Erased copy = erased;
    return copyCount();
  }
};

using IntLambdaCopier = LambdaCopier<int>;
