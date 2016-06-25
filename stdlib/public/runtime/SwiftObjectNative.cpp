// This file is here only to bring in the parts of SwiftObject.mm that apply
// when not using an objc runtime.
#include "swift/Runtime/Config.h"

#if !SWIFT_OBJC_INTEROP
#include "SwiftObject.mm"
#endif
