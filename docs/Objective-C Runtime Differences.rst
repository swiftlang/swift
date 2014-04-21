=========================================
Swift and Objective-C Runtime Differences
=========================================

This document tracks the differences between the Swift and Objective-C runtimes.

Customized Allocation Logic
===========================

Overriding +allocWithZone and +alloc in Objective-C classes that inherit from
Swift classes is not supported and will likely crash.

Proxying
========

Swift classes do not support proxying. Proxying in Swift works if and only if
one does a cast to an Objective-C type.

Alignment
=========

In very rare scenarios, Swift classes allocated in Objective-C code may not be
correctly aligned. This is a bug but not a regression from Objective-C.
Workaround: do not use types that require more than 16-byte alignment as
class instance variables.
