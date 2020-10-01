@import ObjectiveC;

@protocol ObjcProt
  -(void) someFunctionFromProt;

  -(void) someFunctionFromProt2;

@optional
  -(void) someOptionalFunctionFromProt;
@end

@protocol AnotherObjcProt
  -(void) anotherFunctionFromProt;
  -(void) anotherFunctionFromProt2;
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
