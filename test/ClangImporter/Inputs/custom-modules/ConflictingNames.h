@import Foundation;

@protocol OMWWiggle
-(void)conflicting1 __attribute__((swift_name("wiggle1()"))); // expected-error {{'swift_name' and 'swift_name' attributes are not compatible}}
@end

@protocol OMWWaggle
-(void)conflicting1 __attribute__((swift_name("waggle1()"))); // expected-note {{conflicting attribute is here}}
@end

@interface OMWSuper : NSObject <OMWWiggle>
@end

@interface OMWSub : OMWSuper <OMWWaggle>
-(void)conflicting1;
@end
