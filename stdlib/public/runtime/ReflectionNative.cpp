// This file is here only to bring in the parts of Reflection.mm that apply
// when not using an objc runtime.
#include "swift/Runtime/Config.h"

#if !SWIFT_OBJC_INTEROP
#include "Reflection.mm"
#endif
