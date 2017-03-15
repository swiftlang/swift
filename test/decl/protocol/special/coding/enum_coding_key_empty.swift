// RUN: %target-run-simple-swift

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

// Cannot create an enum with a raw value but no cases.
enum EmptyEnum : CodingKey {}

// Cannot check accessors since there are no instances of EmptyEnum to test on.
guard EmptyEnum(stringValue: "") == nil else { fatalError() }
guard EmptyEnum(intValue: 0) == nil else { fatalError() }
