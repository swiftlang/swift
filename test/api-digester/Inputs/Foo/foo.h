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
@end

@interface PhotoBracketSettings : PhotoSettings
+ (instancetype)photoBracketSettingsWithRawPixelFormatType:(int)rawPixelFormatType processedFormat:(int)processedFormat;
+ (instancetype)photoBracketSettingsWithNumber:(int)number;
@end
