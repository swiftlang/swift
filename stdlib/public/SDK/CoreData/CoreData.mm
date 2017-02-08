//===----------------------------------------------------------------------===//
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

#import <CoreData/CoreData.h>

// Make sure -conformsToProtocol: checks succeed even on older OSs.
// This only works because NSFetchRequestResult doesn't have any method
// requirements.

#define CONFORMS_TO_NSFETCHREQUESTRESULT(TYPE) \
@interface TYPE (_SwiftNSFetchRequestResult) <NSFetchRequestResult> \
@end \
@implementation TYPE (_SwiftNSFetchRequestResult) \
@end

CONFORMS_TO_NSFETCHREQUESTRESULT(NSNumber)
CONFORMS_TO_NSFETCHREQUESTRESULT(NSDictionary)
CONFORMS_TO_NSFETCHREQUESTRESULT(NSManagedObject)
CONFORMS_TO_NSFETCHREQUESTRESULT(NSManagedObjectID)
