#import <Foundation/Foundation.h>

void noescapeBlock(__attribute__((noescape)) void (^block)(void));
void noescapeBlock3(__attribute__((noescape)) void (^block)(NSString *s),
                    __attribute__((noescape)) void (^block2)(NSString *s),
                    NSString *f);
