#import <Foundation/NSObject.h>
#import <Foundation/NSUUID.h>

__attribute__((objc_subclassing_restricted))
__attribute__((swift_name("TricksyClass")))
__attribute__((external_source_symbol(language="Swift", defined_in="MyModule", generated_declaration)))
@interface CPTricksyClass : NSObject

@end

@protocol P
@property(readonly) NSUUID *uuid;
@end

@interface CPTricksyClass() <P>
@end
