@import ObjectiveC;

@protocol ObjcProt
  -(void) someFunctionFromProt;
@end

@protocol AnotherObjcProt
  -(void) anotherFunctionFromProt;
@end

@interface ClangInterface: NSObject <ObjcProt>
- (void)someFunction;
@end

@interface PhotoSettings: NSObject
+ (instancetype)photoSettingsWithFormat:(int)format;
+ (instancetype)photoSettingsWithNumber:(int)number;

+ (instancetype)photoSettingsWithCake:(int)cake;
- (instancetype)initWithCake:(int)cake;
@end

@interface PhotoBracketSettings : PhotoSettings
+ (instancetype)photoBracketSettingsWithRawPixelFormatType:(int)rawPixelFormatType processedFormat:(int)processedFormat;
+ (instancetype)photoBracketSettingsWithNumber:(int)number;
@end
