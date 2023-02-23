// Enum usage that is bitwise-able and assignable in C++, aka how CF_OPTIONS
// does things.
typedef int __attribute__((availability(swift, unavailable))) NSEnumerationOptions;
enum : NSEnumerationOptions { NSEnumerationConcurrent, NSEnumerationReverse };

@interface NSSet
- (void)enumerateObjectsWithOptions:(NSEnumerationOptions)opts ;
@end

typedef int __attribute__((availability(swift, unavailable))) NSOrderedCollectionDifferenceCalculationOptions;
enum : NSOrderedCollectionDifferenceCalculationOptions {
  NSOrderedCollectionDifferenceCalculationOptions1,
  NSOrderedCollectionDifferenceCalculationOptions2
};

typedef int __attribute__((availability(swift, unavailable))) NSCalendarUnit;
enum : NSCalendarUnit { NSCalendarUnit1, NSCalendarUnit2 };

typedef int __attribute__((availability(swift, unavailable))) NSSearchPathDomainMask;
enum : NSSearchPathDomainMask { NSSearchPathDomainMask1, NSSearchPathDomainMask2 };

typedef int __attribute__((availability(swift, unavailable))) NSControlCharacterAction;
enum : NSControlCharacterAction { NSControlCharacterAction1, NSControlCharacterAction2 };

typedef int __attribute__((availability(swift, unavailable))) UIControlState;
enum : UIControlState { UIControlState1, UIControlState2 };

typedef int __attribute__((availability(swift, unavailable))) UITableViewCellStateMask;
enum : UITableViewCellStateMask { UITableViewCellStateMask1, UITableViewCellStateMask2 };

typedef int __attribute__((availability(swift, unavailable))) UIControlEvents;
enum : UIControlEvents { UIControlEvents1, UIControlEvents2 };

typedef int __attribute__((availability(swift, unavailable)))
UITableViewScrollPosition;
enum : UITableViewScrollPosition {
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
