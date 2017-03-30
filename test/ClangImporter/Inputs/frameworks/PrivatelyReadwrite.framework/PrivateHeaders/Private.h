#import <PrivatelyReadwrite/PrivatelyReadwrite.h>

@interface PrivateSubclass : Base
@end

@interface PropertiesInit ()
@property (readwrite, nullable) Base *nullabilityChange;
@property (readwrite, nonnull) GenericClass *missingGenerics;
@property (readwrite, nonnull) PrivateSubclass *typeChange;
@end

@interface PropertiesNoInit ()
@property (readwrite, nullable) Base *nullabilityChange;
@property (readwrite, nonnull) GenericClass *missingGenerics;
@property (readwrite, nonnull) PrivateSubclass *typeChange;
@end

@interface PropertiesInitCategory ()
@property (readwrite, nullable) Base *nullabilityChange;
@property (readwrite, nonnull) GenericClass *missingGenerics;
@property (readwrite, nonnull) PrivateSubclass *typeChange;
@end

@interface PropertiesNoInitCategory ()
@property (readwrite, nullable) Base *nullabilityChange;
@property (readwrite, nonnull) GenericClass *missingGenerics;
@property (readwrite, nonnull) PrivateSubclass *typeChange;
@end
