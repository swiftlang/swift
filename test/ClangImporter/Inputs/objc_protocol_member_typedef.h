@import Foundation;

@protocol ParentChildProtocol <NSObject>
- (void)play;
@end

@protocol Parent;

// A typedef can be imported as a typealias member of an Objective-C protocol.
typedef id<ParentChildProtocol> ParentChild __attribute__((swift_name("Parent.Child"))); // expected-note {{'ParentChild' was obsoleted in Swift 3}}

@protocol Parent <NSObject>
@property (nonatomic, readonly, nonnull) ParentChild child;
- (void)adoptChild:(nonnull ParentChild)child;
@end

// Uses of the typedef outside of the protocol.
void ParentUseChild(ParentChild _Nonnull child);

@interface ParentHolder : NSObject
@property (nonatomic, nullable) ParentChild heldChild;
@end

// Swift does not allow other types to be nested in a protocol, and members
// other than typealiases cannot be imported into one, so these are rejected.
struct __attribute__((swift_name("Parent.Record"))) ParentRecord { int value; }; // expected-warning {{imported declaration 'ParentRecord' could not be mapped to 'Parent.Record'}}

__attribute__((swift_name("Parent.Nested"))) // expected-warning {{imported declaration 'ParentNested' could not be mapped to 'Parent.Nested'}}
@protocol ParentNested
@end

typedef NSString *ParentKey __attribute__((swift_name("Parent.Key"))) __attribute__((swift_newtype(struct))); // expected-warning {{imported declaration 'ParentKey' could not be mapped to 'Parent.Key'}}

void ParentDoSomething(id<Parent> _Nonnull parent) __attribute__((swift_name("Parent.doSomething(self:)"))); // expected-warning {{imported declaration 'ParentDoSomething' could not be mapped to 'Parent.doSomething(self:)'}}
