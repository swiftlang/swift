#import <PrivatelyReadwrite/PrivatelyReadwrite.h>

@interface PrivateSubclass : Base
@end

@interface PropertiesA ()
@property (readwrite, nullable) Base *nullabilityChange;
@property (readwrite, nonnull) GenericClass *missingGenerics;
@property (readwrite, nonnull) PrivateSubclass *typeChange;
@end

@interface PropertiesB ()
@property (readwrite, nullable) Base *nullabilityChange;
@property (readwrite, nonnull) GenericClass *missingGenerics;
@property (readwrite, nonnull) PrivateSubclass *typeChange;
@end
