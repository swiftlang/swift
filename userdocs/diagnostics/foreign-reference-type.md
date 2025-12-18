# Foreign reference types (ForeignReferenceType)

Warnings related to foreign reference types imported from C and C++.

## Overview

Swift imports C and C++ types annotated with `SWIFT_SHARED_REFERENCE` as
*foreign reference types* (FRTs). Their memory is managed in a similar manner to
native Swift classes: the compiler automatically inserts calls to retain and
release operations that maintain an FRT's reference count.

When a Swift program calls a foreign function returning an FRT, the compiler
relies on ownership convention annotations to determine whether it needs to
insert a retain call on behalf of the Swift program:

- `SWIFT_RETURNS_RETAINED` means the FRT is returned as an owned object (+1 reference count)
- `SWIFT_RETURNS_UNRETAINED` means the FRT is returned as an unowned object (+0 reference count)

Without these annotations, the compiler can generate code with a superfluous or
missing retain call that leads to memory leaks and errors, so it emits the
following warning:

```
warning: cannot infer ownership of foreign reference value returned by 'returnsFRT()'
```

## Fixing the warning

To address these warnings, you should add the appropriate annotation to your
C or C++ function declaration:

```cpp
// Returns a retained (+1) object
SWIFT_RETURNS_RETAINED MyFRT* createObject();

// Returns an unretained (+0) object
SWIFT_RETURNS_UNRETAINED MyFRT* getExistingObject();
```

Objective-C and Objective-C++ methods can be similarly annotated:

```objc
// Returns a retained (+1) object
- (MyFRT *)createObject SWIFT_RETURNS_RETAINED;

// Returns an unretained (+0) object
- (MyFRT *)getExistingObject SWIFT_RETURNS_UNRETAINED;
```
