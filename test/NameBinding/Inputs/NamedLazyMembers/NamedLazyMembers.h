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

