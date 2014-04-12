// RUN: %swift-ide-test -print-module -source-filename %s -module-to-print=Foundation.NSArray -function-definitions=false > %t/Foundation.NSArray.printed.txt
// RUN: FileCheck -input-file %t/Foundation.NSArray.printed.txt -check-prefix=CHECK1 %s

// CHECK1: class NSMutableArray : NSArray
