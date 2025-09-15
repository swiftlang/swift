#include "CFAvailability.h"

typedef unsigned long NSUInteger;

// Enum usage that is bitwise-able and assignable in C++, aka how CF_OPTIONS
// does things.
typedef CF_OPTIONS(NSUInteger, NSEnumerationOptions) {
  NSEnumerationConcurrent = (1UL << 0),
  NSEnumerationReverse = (1UL << 1),
};

@interface NSSet
- (void)enumerateObjectsWithOptions:(NSEnumerationOptions)opts ;
@end

typedef CF_OPTIONS(NSUInteger, NSOrderedCollectionDifferenceCalculationOptions) {
  NSOrderedCollectionDifferenceCalculationOptions1,
  NSOrderedCollectionDifferenceCalculationOptions2
};

typedef CF_OPTIONS(NSUInteger, NSCalendarUnit) {
  NSCalendarUnit1,
  NSCalendarUnit2
};

typedef CF_OPTIONS(NSUInteger, NSSearchPathDomainMask) {
  NSSearchPathDomainMask1,
  NSSearchPathDomainMask2
};

typedef CF_OPTIONS(NSUInteger, NSControlCharacterAction) {
  NSControlCharacterAction1,
  NSControlCharacterAction2
};

typedef CF_OPTIONS(NSUInteger, UIControlState) {
  UIControlState1,
  UIControlState2
};

typedef CF_OPTIONS(NSUInteger, UITableViewCellStateMask) {
  UITableViewCellStateMask1,
  UITableViewCellStateMask2
};

typedef CF_OPTIONS(NSUInteger, UIControlEvents) {
  UIControlEvents1,
  UIControlEvents2
};

typedef CF_OPTIONS(NSUInteger, UITableViewScrollPosition) {
  UITableViewScrollPosition1,
  UITableViewScrollPosition2
};
@interface NSIndexPath
@end

@interface TestsForEnhancedOmitNeedlessWords
- (void)differenceFromArray:(int)other withOptions:(NSOrderedCollectionDifferenceCalculationOptions)options ;
- (unsigned)minimumRangeOfUnit:(NSCalendarUnit)unit;
- (unsigned)URLForDirectory:(unsigned)directory inDomain:(NSSearchPathDomainMask)domain ;
- (unsigned)layoutManager:(unsigned)layoutManager shouldUseAction:(NSControlCharacterAction)action ;
- (void)setBackButtonBackgroundImage:(unsigned)backgroundImage forState:(UIControlState)state ;
- (void)willTransitionToState:(UITableViewCellStateMask)state ;
- (void)addTarget:(nullable id)target
              action:(SEL)action
    forControlEvents:(UIControlEvents)controlEvents;
- (void)scrollToRowAtIndexPath:(NSIndexPath *)indexPath
              atScrollPosition:(UITableViewScrollPosition)scrollPosition;
@end
