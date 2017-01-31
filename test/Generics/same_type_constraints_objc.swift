// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

@objc protocol Q {}

struct X1<T: AnyObject> { }

extension X1 where T == Q { } 
