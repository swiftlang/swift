// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=Foundation.NSArray -function-definitions=false > %t/Foundation.NSArray.printed.txt
// RUN: FileCheck -input-file %t/Foundation.NSArray.printed.txt -check-prefix=CHECK1 %s

// CHECK1: class NSMutableArray : NSArray
// CHECK1:   func setArray(otherArray: AnyObject[]!)

// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=Foundation.NSKeyValueCoding -function-definitions=false -print-regular-comments > %t/Foundation.NSKeyValueCoding.printed.txt
// RUN: FileCheck -input-file %t/Foundation.NSKeyValueCoding.printed.txt -check-prefix=CHECK2 %s

// CHECK2: extension NSObject
