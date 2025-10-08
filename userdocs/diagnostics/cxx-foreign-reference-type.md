# C++ Foreign Reference Type warnings (CxxForeignReferenceType)

Warnings related to C++ APIs returning foreign reference types that lack proper ownership annotations.

## Overview

When importing C++ types marked with `SWIFT_SHARED_REFERENCE` into Swift, the compiler needs to understand the ownership semantics of functions returning these types. Without explicit annotations, the compiler cannot determine whether the returned object should be retained or not, which can lead to memory management issues.

## The Warning

The compiler emits a warning when it encounters a C++ function or Objective-C method that:
1. Returns a type marked with `SWIFT_SHARED_REFERENCE` (foreign reference type)
2. Lacks either `SWIFT_RETURNS_RETAINED` or `SWIFT_RETURNS_UNRETAINED` annotation

Example warning:
```
warning: cannot infer the ownership of the returned value, annotate 'functionName()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED
```

## How to Fix

### For C++ Functions

Add the appropriate annotation to your C++ function declaration:

```cpp
// If the function returns a retained value
SWIFT_RETURNS_RETAINED MyFRT* createObject();

// If the function returns an unretained value
SWIFT_RETURNS_UNRETAINED MyFRT* getExistingObject();
```

### For Objective-C Methods

Similarly, annotate your Objective-C methods:

```objc
// Retained value
- (MyFRT *)createObject SWIFT_RETURNS_RETAINED;

// Unretained value
- (MyFRT *)getExistingObject SWIFT_RETURNS_UNRETAINED;
```
