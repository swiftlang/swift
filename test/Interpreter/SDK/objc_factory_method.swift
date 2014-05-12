// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-build-swift -module-name FactoryTest -Xfrontend -enable-objc-factory-method-constructors %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s
// REQUIRES: OS=macosx

import AppKit

let service = NSSharingService(named: NSSharingServiceNamePostOnTwitter)
// CHECK: Twitter
println(service.title)
