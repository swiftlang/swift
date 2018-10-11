// RUN: %target-typecheck-verify-swift
// REQUIRES: dispatch

import Dispatch

let q = dispatch_queue_create("", nil) // expected-error {{'dispatch_queue_create' has been replaced by 'DispatchQueue.init(label:qos:attributes:autoreleaseFrequency:target:)'}}

_ = dispatch_queue_get_label(q) // expected-error {{'dispatch_queue_get_label' has been replaced by property 'DispatchQueue.label'}}

_ = dispatch_get_global_queue(0, 0) // expected-error {{'dispatch_get_global_queue' has been replaced by 'DispatchQueue.global(attributes:)'}}

_ = dispatch_source_create(OpaquePointer(bitPattern: ~0)!, 0, 0, nil) // expected-error {{'dispatch_source_create' is unavailable: Use DispatchSource class methods}}

// Next test is disabled pending what to do about libdispatch change to the signature of dispatch_get_main_queue(): rdar://problem/36528231.
// To re-enable, remove the #if/#endif and remove "FIXME-" from 'expected-FIXME-error'
#if false
_ = dispatch_get_main_queue() // expected-FIXME-error {{'dispatch_get_main_queue()' has been replaced by property 'DispatchQueue.main'}}
#endif
_ = DispatchQueue.main
