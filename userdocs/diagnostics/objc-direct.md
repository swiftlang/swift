# Objective-C direct members (ObjCDirect)

## Overview

A direct member (`@objcDirect` in Swift, or `__attribute__((objc_direct))` in an
Objective-C header) is called by its symbol name. It has no method list, entry,
so nothing that looks it up at runtime can find it, and cannot be overriden. This
group contains the diagnostics for code that relies on either.

In a module that enables the `ObjCDirect` experimental feature, these
diagnostics are errors. In one that doesn't, they're off by default warnings,
such that they don't break existing code base. Enable them with
`-Wwarning ObjCDirect`, or make them errors with `-Werror ObjCDirect`.

## Uses that need dynamic dispatch

```swift
// Bar.h: - (NSString *)directMethod __attribute__((objc_direct));
//        @property (direct) NSInteger level;

_ = #selector(Bar.directMethod)            // no method answers the selector
_ = #keyPath(Bar.level)                    // key-value coding can't find it
_ = bar.observe(\.level) { _, _ in }       // key-value observing can't either

@objc protocol Describing { func directMethod() -> String! }
extension Bar: Describing {}               // calls through the protocol fail
```

These diagnostics cover `#selector`, `#keyPath`, a key path literal passed to a
key-value observing or coding API such as `observe(_:options:changeHandler:)`,
`publisher(for:options:)` or `NSSortDescriptor(keyPath:ascending:)`, and
witnessing an `@objc` protocol requirement.

To resolve these diagnostics, do one of the following:
- Call the member directly rather than through a selector, a key path string or
  a protocol.
- If something has to find it at runtime, keep a member that is not direct for
  that purpose, for example an `@objc` method that calls the direct one.
- To observe changes to a direct property, use something other than key-value
  observing, such as a `didSet` observer, Combine or Observation.

## Direct initializers

A subclass can neither override nor hide a direct initializer. So a call such
as

```objc
Sub *s = [[Sub alloc] initWithValue:5];
```

runs the superclass's initializer on an instance of `Sub`. A designated
initializer does the work itself, so none of `Sub`'s initialization runs: its
stored properties keep their zero-filled memory, and a non-optional reference
reads as `nil`, which is undefined behavior. A convenience initializer
delegates with `self.init` to an initializer `Sub` can override, so `Sub`'s
initialization still runs. Two rules follow.

A direct designated initializer requires a `final` class:

```swift
class Widget: NSObject {
  @objcDirect init(size: Int) { ... }
  // error: a designated '@objcDirect' initializer requires class 'Widget' to be 'final'
}
```

A direct convenience initializer is allowed in any class, but every
`self.init` in it must reach an initializer every subclass can override. In a
Swift class, that's one every subclass can see: `public` in an `open` class,
at least `internal` otherwise. A `private` or `fileprivate` one would run on
the subclass and skip its initialization:

```swift
class Gadget: NSObject {
  init(size: Int) { ... }
  private init(raw: Int) { ... }
  @objcDirect convenience init(name: String) { self.init(size: name.count) }
  @objcDirect convenience init(rawName: String) { self.init(raw: rawName.count) }
  // error: an '@objcDirect' initializer must delegate to an initializer every subclass can override
}
```

A subclass can't declare an initializer with the same name as a direct
convenience initializer, so adding `@objcDirect` to one in an `open` class can
break subclasses in other modules.

In a class declared in Objective-C, the initializer `self.init` reaches must
be one the header declares. An `@objc @implementation` class can't be `final`,
so there a designated initializer can't be direct, in the header or in the
implementation. Mark the designated initializers in the header and leave the
direct one out:

```objc
#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN
@interface Widget : NSObject
- (instancetype)initWithSize:(NSInteger)size NS_DESIGNATED_INITIALIZER;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithName:(NSString *)name __attribute__((objc_direct));
@end
NS_ASSUME_NONNULL_END
```

```swift
@objc @implementation extension Widget {
  init(size: Int) { super.init() }
  @objcDirect convenience init(name: String) { self.init(size: name.count) }
}
```

Swift adds no rules for an initializer that is declared direct in an
Objective-C header and implemented in Objective-C. Swift subclasses of its
class inherit initializers as usual, and Swift doesn't diagnose the ways the
initializer can run on a subclass instance and skip the subclass's
initialization: from Objective-C, from a factory method or a convenience
initializer that calls it, or from a Swift class method that returns `Self`.

If such an initializer is designated, a generated header can't declare a Swift
subclass for Objective-C: it redeclares the initializer, and Clang rejects that
with "cannot override a method that is declared direct by a superclass". Keep
such a subclass out of the header, for example by making it `private` or
`fileprivate`; an app target's header declares `internal` classes too.

With the feature enabled, `self.init` delegation to a direct initializer
imported from Objective-C is a static call if its header declares it as a
convenience initializer, also through the copy a subclass gets from the
importer. Delegation to a designated one sends a message, as without the
feature, and fails at run time with "unrecognized selector" unless the
instance's class implements the selector, as a Swift subclass's inherited
override does. To make it a convenience initializer, mark all of the class's
designated initializers `NS_DESIGNATED_INITIALIZER` and leave it out.

To resolve these diagnostics, do one of the following:
- Make the Swift class `final`, make the direct initializer a convenience
  initializer, or replace `@objcDirect` on it with `@objc`.
- In an `@objc @implementation`, make the direct initializer a convenience
  initializer: mark all of the class's designated initializers
  `NS_DESIGNATED_INITIALIZER` in the header and leave the direct one out, and
  add `convenience` to its implementation. Or remove `objc_direct` from it.
  If `objc_direct_members` makes a designated initializer direct, declare that
  initializer in a class extension without `objc_direct_members`.
- Delegate from a direct convenience initializer to an initializer every
  subclass can override: in a class declared in Objective-C, one the header
  declares; in a Swift class, one that is at least `internal`, or `public` in
  an `open` class.
