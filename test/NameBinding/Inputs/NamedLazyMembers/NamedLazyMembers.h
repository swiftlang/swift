#import <Foundation/Foundation.h>

@protocol Doer

- (void)doSomeWork;
- (void)doSomeWorkWithSpeed:(int)s;
- (void)doSomeWorkWithSpeed:(int)s thoroughness:(int)t
  NS_SWIFT_NAME(doVeryImportantWork(speed:thoroughness:));
- (void)doSomeWorkWithSpeed:(int)s alacrity:(int)a
  NS_SWIFT_NAME(doSomeWorkWithSpeed(speed:levelOfAlacrity:));

// These we are generally trying to not-import, via laziness.
- (void)goForWalk;
- (void)takeNap;
- (void)eatMeal;
- (void)tidyHome;
- (void)callFamily;
- (void)singSong;
- (void)readBook;
- (void)attendLecture;
- (void)writeLetter;

@end


// Don't conform to the protocol; that loads all protocol members.
@interface SimpleDoer

- (instancetype)initWithValue: (int)value;

// These are names we're hoping don't interfere with Doer, above.
+ (SimpleDoer*)Doer;
+ (SimpleDoer*)DoerOfNoWork;

- (void)simplyDoSomeWork;
- (void)simplyDoSomeWorkWithSpeed:(int)s;
- (void)simplyDoSomeWorkWithSpeed:(int)s thoroughness:(int)t
  NS_SWIFT_NAME(simplyDoVeryImportantWork(speed:thoroughness:));
- (void)simplyDoSomeWorkWithSpeed:(int)s alacrity:(int)a
  NS_SWIFT_NAME(simplyDoSomeWorkWithSpeed(speed:levelOfAlacrity:));

// These we are generally trying to not-import, via laziness.
- (void)simplyGoForWalk;
- (void)simplyTakeNap;
- (void)simplyEatMeal;
- (void)simplyTidyHome;
- (void)simplyCallFamily;
- (void)simplySingSong;
- (void)simplyReadBook;
- (void)simplyAttendLecture;
- (void)simplyWriteLetter;

@end


// Don't conform to the protocol; that loads all protocol members.
@interface SimpleDoer (Category)
- (void)categoricallyDoSomeWork;
- (void)categoricallyDoSomeWorkWithSpeed:(int)s;
- (void)categoricallyDoSomeWorkWithSpeed:(int)s thoroughness:(int)t
  NS_SWIFT_NAME(categoricallyDoVeryImportantWork(speed:thoroughness:));
- (void)categoricallyDoSomeWorkWithSpeed:(int)s alacrity:(int)a
  NS_SWIFT_NAME(categoricallyDoSomeWorkWithSpeed(speed:levelOfAlacrity:));

// These we are generally trying to not-import, via laziness.
- (void)categoricallyGoForWalk;
- (void)categoricallyTakeNap;
- (void)categoricallyEatMeal;
- (void)categoricallyTidyHome;
- (void)categoricallyCallFamily;
- (void)categoricallySingSong;
- (void)categoricallyReadBook;
- (void)categoricallyAttendLecture;
- (void)categoricallyWriteLetter;
@end


@protocol MirroredBase
+ (void)mirroredBaseClassMethod;
- (void)mirroredBaseInstanceMethod;
@end

@protocol MirroredDoer <MirroredBase>
+ (void)mirroredDerivedClassMethod;
- (void)mirroredDerivedInstanceMethod;
@end

@interface MirroringDoer : NSObject<MirroredDoer>
- (void)unobtrusivelyGoForWalk;
- (void)unobtrusivelyTakeNap;
- (void)unobtrusivelyEatMeal;
- (void)unobtrusivelyTidyHome;
- (void)unobtrusivelyCallFamily;
- (void)unobtrusivelySingSong;
- (void)unobtrusivelyReadBook;
- (void)unobtrusivelyAttendLecture;
- (void)unobtrusivelyWriteLetter;
@end

@interface DerivedFromMirroringDoer : MirroringDoer
@end

@interface SimilarlyNamedThings
- (void)doSomething:(double)x;
- (void)doSomething:(double)x celsius:(double)y;
- (void)doSomething:(double)x fahrenheit:(double)y using:(void (^)(void))block;
@end

@interface SimpleDoerSubclass : SimpleDoer
- (void)simplyDoSomeWorkWithSpeed:(int)s thoroughness:(int)t
  NS_SWIFT_NAME(simplyDoVeryImportantWork(speed:thoroughness:));

- (void)exuberantlyGoForWalk;
- (void)exuberantlyTakeNap;
- (void)exuberantlyEatMeal;
- (void)exuberantlyTidyHome;
- (void)exuberantlyCallFamily;
- (void)exuberantlySingSong;
- (void)exuberantlyReadBook;
- (void)exuberantlyAttendLecture;
- (void)exuberantlyWriteLetter;
@end
