// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

import Dispatch

let q = dispatch_queue_create("", nil) // expected-error {{'dispatch_queue_create' has been replaced by 'DispatchQueue.init(label:qos:attributes:autoreleaseFrequency:target:)'}}

_ = dispatch_queue_get_label(q) // expected-error {{'dispatch_queue_get_label' has been replaced by property 'DispatchQueue.label'}}

_ = dispatch_get_global_queue(0, 0) // expected-error {{'dispatch_get_global_queue' has been replaced by 'DispatchQueue.global(attributes:)'}}

_ = dispatch_source_create(OpaquePointer(bitPattern: ~0)!, 0, 0, nil) // expected-error {{'dispatch_source_create' is unavailable: Use DispatchSource class methods}}

_ = dispatch_get_main_queue() // expected-error {{'dispatch_get_main_queue()' has been replaced by property 'DispatchQueue.main'}}
_ = DispatchQueue.main
