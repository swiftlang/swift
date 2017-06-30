#import <Foundation/NSString.h>

// An NSString whose _fastCharacterContents always returns nil
@interface NSSlowString : NSString

@property (nonatomic, strong) id myProperty;

- (void *) _fastCharacterContents;

@end
