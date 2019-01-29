@import Foundation;

#ifndef NS_SWIFT_NAME(Name)
#  define NS_SWIFT_NAME(Name) __attribute__((swift_name(#Name)))
#endif

@interface SEGreebieArray : NSObject
@end

typedef NS_OPTIONS(NSUInteger, OMWWobbleOptions) {
  OMWWobbleSideToSide = 0x01,
  OMWWobbleBackAndForth = 0x02,
  OMWWobbleToXMLHex = 0x04
};

@interface OmitNeedlessWords : NSObject
-(void)jumpToUrl:(nonnull NSURL *)url;
-(void)jumpToGuid:(nonnull NSGUID *)guid;
-(void)jumpAgainToGUID:(nonnull NSGUID *)guid;
-(BOOL)objectIsCompatibleWithObject:(nonnull id)other;
-(void)insetByX:(NSInteger)x y:(NSInteger)y;
-(void)setIndirectlyToValue:(nonnull id)object;
-(void)jumpToTop:(nonnull id)sender;
-(void)removeWithNoRemorse:(nonnull id)object;
-(void)bookmarkWithURLs:(nonnull NSArray<NSURL *> *)urls;
-(void)bookmarkWithGUIDs:(nonnull NSArray<NSGUID *> *)guids;
-(void)saveToURL:(nonnull NSURL *)url forSaveOperation:(NSInteger)operation;
-(void)saveToGUID:(nonnull NSGUID *)guid forSaveOperation:(NSInteger)operation;
-(void)indexWithItemNamed:(nonnull NSString *)name;
-(void)methodAndReturnError:(NSError **)error;
-(nullable Class)typeOfString:(nonnull NSString *)string;
-(nullable Class)typeOfNamedString:(nonnull NSString *)string;
-(nullable Class)typeOfTypeNamed:(nonnull NSString *)string;
-(void)appendWithContentsOfString:(nonnull NSString *)string;
-(nonnull id)objectAtIndexedSubscript:(NSUInteger)idx;
-(void)exportPresetsBestMatchingString:(nonnull NSString *)string;
-(void)isCompatibleWithString:(nonnull NSString *)string;
-(void)addObjectValue:(nonnull id)object;
-(nonnull OmitNeedlessWords *)wordsBySlobbering:(nonnull NSString *)string;
-(void)drawPolygonWithPoints:(const NSPoint[])points count:(NSInteger)count;
-(void)drawFilledPolygonWithPoints:(NSPointArray)points count:(NSInteger)count;
-(void)drawGreebies:(nonnull SEGreebieArray*)greebies;
-(void)doSomethingBoundBy:(NSInteger)value;
-(void)doSomethingSeparatedBy:(NSInteger)value;
+(nonnull OmitNeedlessWords *)currentOmitNeedlessWords;
+(void)setCurrentOmitNeedlessWords:(nonnull OmitNeedlessWords *)value;
-(void)compilerPlugInValue:(NSInteger)value;
-(void)wobbleWithOptions:(OMWWobbleOptions)options;
@end

@interface ABCDoodle : NSObject
@property (nonatomic,copy,nonnull) NSArray<ABCDoodle *> *doodles;
-(void)addDoodle:(nonnull ABCDoodle *)doodle;
@end

@protocol OMWLanding
-(void)flipLanding;
@end

@protocol OMWWiggle
-(void)joinSub;
-(void)conflicting1 NS_SWIFT_NAME(wiggle1());
@property (readonly) NSInteger conflictingProp1 NS_SWIFT_NAME(wiggleProp1);
@end

@protocol OMWWaggle
-(void)conflicting1 NS_SWIFT_NAME(waggle1());
@property (readonly) NSInteger conflictingProp1 NS_SWIFT_NAME(waggleProp1);
@end

@interface OMWSuper : NSObject <OMWWiggle>
-(void)jumpSuper;
@property (readonly) NSInteger conflictingProp1;
@end

@interface OMWSub : OMWSuper <OMWWaggle>
-(void)jumpSuper;
-(void)joinSub;
-(void)conflicting1;
@property (readonly) NSInteger conflictingProp1;
@end
