// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify -import-objc-header %t/Widget.h \
// RUN:   -verify-additional-file %t/Widget.h %t/main.swift \
// RUN:   -enable-experimental-feature ObjCDirect

// In Swift 5 mode, 'self' has type 'Self', which changes how 'self.init' looks.
// RUN: %target-swift-frontend -typecheck -verify -import-objc-header %t/Widget.h \
// RUN:   -verify-additional-file %t/Widget.h %t/main.swift \
// RUN:   -enable-experimental-feature ObjCDirect -swift-version 5

// Subclassing a class with a direct initializer changes nothing, with or
// without the feature.
// RUN: %target-swift-frontend -typecheck -verify -import-objc-header %t/Widget.h \
// RUN:   %t/subclass.swift -enable-experimental-feature ObjCDirect
// RUN: %target-swift-frontend -typecheck -verify -import-objc-header %t/Widget.h \
// RUN:   %t/subclass.swift

//--- Widget.h

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN
@interface Widget : NSObject
- (instancetype)initWithSize:(NSInteger)size NS_DESIGNATED_INITIALIZER;
- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithName:(NSString *)name __attribute__((objc_direct));
- (instancetype)initWithNick:(NSString *)nick __attribute__((objc_direct));
- (instancetype)initWithRawName:(NSString *)name __attribute__((objc_direct));
- (instancetype)initWithCount:(NSInteger)count __attribute__((objc_direct));
- (instancetype)initWithEither:(NSInteger)either __attribute__((objc_direct));
- (instancetype)initWithAlias:(NSString *)alias __attribute__((objc_direct));
- (instancetype)initWithSeed:(NSInteger)seed __attribute__((objc_direct))
    NS_DESIGNATED_INITIALIZER;
// expected-note@-2 {{remove 'NS_DESIGNATED_INITIALIZER' from 'init(seed:)'; 'Widget' needs a designated initializer that isn't direct}}
@end

// With no designated initializers marked, every initializer is one, a
// category's too.
@interface Gadget : NSObject
- (instancetype)initWithTag:(NSInteger)tag __attribute__((objc_direct));
// expected-note@-1 {{'Gadget' marks no designated initializers, so 'init(tag:)' is one; mark the designated ones 'NS_DESIGNATED_INITIALIZER', which can't be direct, and leave this one out}}
@end
@interface Gadget (Named)
- (instancetype)initWithLabel:(NSString *)label __attribute__((objc_direct));
// expected-note@-1 {{'Gadget' marks no designated initializers, so 'init(label:)' is one}}
@end

// 'objc_direct_members' makes the designated initializer direct too, or,
// with none marked, every initializer.
__attribute__((objc_direct_members))
@interface Sprocket : NSObject
- (instancetype)initWithSize:(NSInteger)size NS_DESIGNATED_INITIALIZER;
// expected-note@-1 {{'objc_direct_members' makes 'init(size:)' direct; declare the designated initializers of 'Sprocket' in a class extension without 'objc_direct_members'}}
- (instancetype)init NS_UNAVAILABLE;
@end
__attribute__((objc_direct_members))
@interface Doohickey : NSObject
- (instancetype)initWithSize:(NSInteger)size;
// expected-note@-1 {{'objc_direct_members' makes 'init(size:)' direct; declare the designated initializers of 'Doohickey' in a class extension without 'objc_direct_members'}}
@end

@interface Base : NSObject
- (instancetype)initWithSeed:(NSInteger)seed __attribute__((objc_direct))
    NS_DESIGNATED_INITIALIZER;
- (instancetype)init NS_DESIGNATED_INITIALIZER;
// Runs '[self initWithSeed:]'.
- (instancetype)initWithCount:(NSInteger)count;
@end

// Gets Base's initializers as convenience initializers.
@interface TaggedMid : Base
- (instancetype)initWithTag:(NSInteger)tag NS_DESIGNATED_INITIALIZER;
@end

@interface ImplSub : Base
@end
NS_ASSUME_NONNULL_END

//--- main.swift

import Foundation

// Clang can't see an initializer a Swift extension adds.
extension NSObject {
  @objc convenience init(alias: String) { self.init() }
  // expected-note@-1 {{overridden declaration is here}}
}

@objc @implementation extension Widget {
  init(size: Int) { super.init() }

  private init(raw: Int) { super.init() }
  // expected-note@-1 2 {{'init(raw:)' isn't in the header, so a subclass can't override it}}

  @objcDirect convenience init(name: String) { self.init(size: name.count) }

  // Another direct initializer the header declares is fine.
  @objcDirect convenience init(nick: String) { self.init(name: nick) }

  @objcDirect convenience init(alias: String) { self.init(size: alias.count) }
  // expected-error@-1 {{an '@objcDirect' initializer cannot override a superclass declaration}}

  @objcDirect convenience init(rawName: String) { self.init(raw: rawName.count) }
  // expected-error@-1 {{an '@objcDirect' initializer must delegate to an initializer every subclass can override}}

  // Every 'self.init' counts.
  @objcDirect convenience init(either: Int) {
    if either > 0 { self.init(size: either) } else { self.init(raw: either) }
    // expected-error@-1 {{an '@objcDirect' initializer must delegate to an initializer every subclass can override}}
  }

  @objcDirect init(count: Int) { super.init() }
  // expected-error@-1 {{a designated initializer in an '@objc @implementation' extension can't be '@objcDirect'}}
  // expected-note@-2 {{a subclass could neither override nor hide it}}
  // expected-note@-3 {{add 'convenience' keyword to make this a convenience initializer}} {{15-15=convenience }}

  @objcDirect convenience init(seed: Int) { self.init(size: seed) }
  // expected-error@-1 {{'init(seed:)' can't be '@objcDirect': the header makes it a designated initializer}}
  // expected-note@-2 {{a subclass could neither override nor hide it}}
}

// 'init()' is inherited from NSObject, whose header declares it.
@objc @implementation extension Gadget {
  @objcDirect convenience init(tag: Int) { self.init() }
  // expected-error@-1 {{'init(tag:)' can't be '@objcDirect': the header makes it a designated initializer}}
  // expected-note@-2 {{a subclass could neither override nor hide it}}
}

@objc(Named) @implementation extension Gadget {
  @objcDirect convenience init(label: String) { self.init() }
  // expected-error@-1 {{'init(label:)' can't be '@objcDirect': the header makes it a designated initializer}}
  // expected-note@-2 {{a subclass could neither override nor hide it}}
}

// The header has to change first, so there's no 'convenience' fix-it.
@objc @implementation extension Sprocket {
  @objcDirect init(size: Int) { super.init() }
  // expected-error@-1 {{a designated initializer in an '@objc @implementation' extension can't be '@objcDirect'}}
  // expected-note@-2 {{a subclass could neither override nor hide it}}
}

@objc @implementation extension Doohickey {
  @objcDirect convenience init(size: Int) { self.init() }
  // expected-error@-1 {{'init(size:)' can't be '@objcDirect': the header makes it a designated initializer}}
  // expected-note@-2 {{a subclass could neither override nor hide it}}
}

//--- subclass.swift

import Foundation

class Sub: Base {
  var helper = NSObject()
}
class TaggedLeaf: TaggedMid {}

extension Base {
  convenience init(double seed: Int) { self.init(seed: seed * 2) }
}

func use() {
  _ = Sub(seed: 1)
  _ = Sub(count: 1)
  _ = Sub(double: 1)
  _ = TaggedLeaf(seed: 1)
}

@objc @implementation extension ImplSub {}
