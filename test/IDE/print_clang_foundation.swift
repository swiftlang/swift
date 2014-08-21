// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=Foundation.NSArray -function-definitions=false -module-cache-path=%t/mcp > %t/Foundation.NSArray.printed.txt
// RUN: FileCheck -input-file %t/Foundation.NSArray.printed.txt -check-prefix=CHECK1 %s

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=Foundation.NSArray -function-definitions=false -enable-objc-failable-initializers -module-cache-path=%t/mcp > %t/Foundation.NSArray-failable-inits.printed.txt
// RUN: FileCheck -input-file %t/Foundation.NSArray-failable-inits.printed.txt -check-prefix=CHECK1-FAILABLE-INITS %s

// CHECK1: class NSMutableArray : NSArray
// CHECK1:   func setArray(otherArray: [AnyObject])

// CHECK1-FAILABLE-INITS: class NSArray
// init()CHECK1-FAILABLE-INITS: init()
// init()CHECK1-FAILABLE-INITS: convenience init?(contentsOfFile path: String)
// init()CHECK1-FAILABLE-INITS: convenience init(contentsOfURL url: NSURL)

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=Foundation.NSKeyValueCoding -function-definitions=false -print-regular-comments -module-cache-path=%t/mcp > %t/Foundation.NSKeyValueCoding.printed.txt
// RUN: FileCheck -input-file %t/Foundation.NSKeyValueCoding.printed.txt -check-prefix=CHECK2 %s

// CHECK2: extension NSObject

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=Foundation.NSString -function-definitions=false -module-cache-path=%t/mcp > %t/Foundation.NSString.printed.txt
// RUN: FileCheck -input-file %t/Foundation.NSString.printed.txt -check-prefix=CHECK_NSSTRING %s
// RUN: FileCheck -input-file %t/Foundation.NSString.printed.txt -check-prefix=CHECK_DICTIONARY %s

// Make sure that we don't qualify 'NSErrorPointer'.
// CHECK_NSSTRING: init(contentsOfFile path: String, encoding enc: UInt, error: NSErrorPointer)

// CHECK_DICTIONARY: func propertyListFromStringsFileFormat() -> [NSObject : AnyObject]
