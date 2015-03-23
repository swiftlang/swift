// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %target-swift-ide-test -print-module -source-filename %s -module-to-print=Foundation.NSArray -function-definitions=false > %t/Foundation.NSArray.printed.txt
// RUN: FileCheck -input-file %t/Foundation.NSArray.printed.txt -check-prefix=CHECK1 %s

// RUN: %target-swift-ide-test -print-module -source-filename %s -module-to-print=Foundation.NSArray -function-definitions=false > %t/Foundation.NSArray-failable-inits.printed.txt
// RUN: FileCheck -input-file %t/Foundation.NSArray-failable-inits.printed.txt -check-prefix=CHECK1-FAILABLE-INITS %s

// REQUIRES: objc_interop

// CHECK1: class NSMutableArray : NSArray
// CHECK1:   func setArray(otherArray: [AnyObject])

// CHECK1-FAILABLE-INITS: class NSArray
// init()CHECK1-FAILABLE-INITS: init()
// init()CHECK1-FAILABLE-INITS: convenience init?(contentsOfFile path: String)
// init()CHECK1-FAILABLE-INITS: convenience init?(contentsOfURL url: NSURL)

// RUN: %target-swift-ide-test -print-module -source-filename %s -module-to-print=Foundation.NSKeyValueCoding -function-definitions=false -print-regular-comments > %t/Foundation.NSKeyValueCoding.printed.txt
// RUN: FileCheck -input-file %t/Foundation.NSKeyValueCoding.printed.txt -check-prefix=CHECK2 %s

// CHECK2: extension NSObject

// RUN: %target-swift-ide-test -print-module -source-filename %s -module-to-print=Foundation.NSString -function-definitions=false > %t/Foundation.NSString.printed.txt
// RUN: FileCheck -input-file %t/Foundation.NSString.printed.txt -check-prefix=CHECK_NSSTRING %s
// RUN: FileCheck -input-file %t/Foundation.NSString.printed.txt -check-prefix=CHECK_DICTIONARY %s

// Make sure that we don't qualify 'NSErrorPointer'.
// CHECK_NSSTRING: init?(contentsOfFile path: String, encoding enc: UInt, error: NSErrorPointer)

// CHECK_DICTIONARY: func propertyListFromStringsFileFormat() -> [NSObject : AnyObject]

// RUN: %target-swift-ide-test -print-module -source-filename %s -module-to-print=Foundation -function-definitions=false > %t/Foundation.printed.txt
// RUN: FileCheck -input-file %t/Foundation.printed.txt -check-prefix=CHECK_DUP %s

// CHECK_DUP: import CoreFoundation{{$}}
// CHECK_DUP-NOT: import CoreFoundation{{$}}
