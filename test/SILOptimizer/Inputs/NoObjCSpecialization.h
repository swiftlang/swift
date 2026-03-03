#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

extern __attribute__((visibility("default")))
@protocol NoObjCSpecialization<NSObject>

- (BOOL)fooWithValue:(id)value
             andBool:(BOOL)boolValue
               error:(NSError *_Nullable *)error;

@end

NS_ASSUME_NONNULL_END
