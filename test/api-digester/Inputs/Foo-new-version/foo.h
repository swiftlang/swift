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
@end

@interface PhotoBracketSettings : PhotoSettings
+ (instancetype)photoBracketSettingsWithRawPixelFormatType:(int)rawPixelFormatType processedFormat:(int)processedFormat;
+ (instancetype)photoBracketSettingsWithNumber:(int)number;
@end
