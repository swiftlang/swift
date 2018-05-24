//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FOUNDATION_NSERROR_H
#define SWIFT_FOUNDATION_NSERROR_H

#include "swift/Runtime/Metadata.h"
#include "../../runtime/SwiftHashableSupport.h"
#include <Foundation/Foundation.h>

namespace swift {

/// The name of the symbol that ErrorObject.mm will look up using dlsym. It uses
/// this to locate various items related to Error bridging to NS/CFError.
#define ERROR_BRIDGING_SYMBOL_NAME swift_errorBridgingInfo
#define ERROR_BRIDGING_SYMBOL_NAME_STRING "swift_errorBridgingInfo"

/// The items that ErrorObject.mm needs for bridging. The
/// ERROR_BRIDGING_SYMBOL_NAME symbol will contain an instance of this struct.
struct ErrorBridgingInfo {
  const SWIFT_CC(swift) WitnessTable *(*GetCFErrorErrorConformance)();
  
  const SWIFT_CC(swift) hashable_support::HashableWitnessTable *
    (*GetNSObjectHashableConformance)();
  
  SWIFT_CC(swift) NSDictionary *(*GetErrorDefaultUserInfo)(const OpaqueValue *error,
                                                           const Metadata *T,
                                                           const WitnessTable *Error);
  
  SWIFT_CC(swift) bool (*BridgeErrorToNSError)(NSError *, OpaqueValue*, const Metadata *,
                                               const WitnessTable *);
  
  const ProtocolDescriptor *ObjectiveCBridgeableError;
};

}

#endif // SWIFT_FOUNDATION_NSERROR_H
