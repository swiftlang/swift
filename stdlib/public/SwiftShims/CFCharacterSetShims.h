//===--- CFCharacterSetShims.h - CoreFoundation declarations for CF hashing functions ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

CF_IMPLICIT_BRIDGING_ENABLED
CF_EXTERN_C_BEGIN
_Pragma("clang assume_nonnull begin")

CF_EXPORT CFCharacterSetRef _CFURLComponentsGetURLUserAllowedCharacterSet();
CF_EXPORT CFCharacterSetRef _CFURLComponentsGetURLPasswordAllowedCharacterSet();
CF_EXPORT CFCharacterSetRef _CFURLComponentsGetURLHostAllowedCharacterSet();
CF_EXPORT CFCharacterSetRef _CFURLComponentsGetURLPathAllowedCharacterSet();
CF_EXPORT CFCharacterSetRef _CFURLComponentsGetURLQueryAllowedCharacterSet();
CF_EXPORT CFCharacterSetRef _CFURLComponentsGetURLFragmentAllowedCharacterSet();

_Pragma("clang assume_nonnull end")
CF_EXTERN_C_END
CF_IMPLICIT_BRIDGING_DISABLED
