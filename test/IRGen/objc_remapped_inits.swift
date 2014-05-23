// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -target x86_64-apple-darwin10 %s -emit-ir | FileCheck %s

import UIKit

// CHECK-NOT: (initWithTitle:delegate:cancelButtonTitle:destructiveButtonTitle:otherButtonTitles:)
// CHECK: (initWithTitle:delegate:cancelButtonTitle:destructiveButtonTitle:)
// CHECK-NOT: (initWithTitle:delegate:cancelButtonTitle:destructiveButtonTitle:otherButtonTitles:)

_ = UIActionSheet(title: "abc", delegate: nil, cancelButtonTitle: nil, destructiveButtonTitle: nil)
