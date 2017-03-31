#import <PrivatelyReadwrite/PrivatelyReadwrite.h>

@interface PrivateSubclass : Base
@end

@interface PropertiesInit ()
@property (readwrite, nonnull) Base *readwriteChange;
@property (readwrite, nullable) Base *nullabilityChange;
@property (readwrite, nonnull) GenericClass *missingGenerics;
@property (readwrite, nonnull) PrivateSubclass *typeChange;
@end

@interface PropertiesNoInit ()
@property (readwrite, nonnull) Base *readwriteChange;
@property (readwrite, nullable) Base *nullabilityChange;
@property (readwrite, nonnull) GenericClass *missingGenerics;
@property (readwrite, nonnull) PrivateSubclass *typeChange;
@end

@interface PropertiesInitGeneric<T> ()
@property (readwrite, nonnull) Base *readwriteChange;
@property (readwrite, nullable) Base *nullabilityChange;
@property (readwrite, nonnull) GenericClass *missingGenerics;
@property (readwrite, nonnull) PrivateSubclass *typeChange;
@end

@interface PropertiesNoInitGeneric<T> ()
@property (readwrite, nonnull) Base *readwriteChange;
@property (readwrite, nullable) Base *nullabilityChange;
@property (readwrite, nonnull) GenericClass *missingGenerics;
@property (readwrite, nonnull) PrivateSubclass *typeChange;
@end

@interface PropertiesInitCategory ()
@property (readwrite, nonnull) Base *readwriteChange;
@property (readwrite, nullable) Base *nullabilityChange;
@property (readwrite, nonnull) GenericClass *missingGenerics;
@property (readwrite, nonnull) PrivateSubclass *typeChange;
@end

@interface PropertiesNoInitCategory ()
@property (readwrite, nonnull) Base *readwriteChange;
@property (readwrite, nullable) Base *nullabilityChange;
@property (readwrite, nonnull) GenericClass *missingGenerics;
@property (readwrite, nonnull) PrivateSubclass *typeChange;
@end
