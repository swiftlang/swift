#include "swift/bridging"

class SWIFT_UNSAFE_REFERENCE MyUnsafeReferenceType {
public:
  virtual void virtualSafeMethod() SWIFT_SAFE {}
  virtual void virtualUnsafeMethod() {}
  void nonvirtualSafeMethod() SWIFT_SAFE {}
  virtual ~MyUnsafeReferenceType() = default;
};
