#import <Foundation/Foundation.h>
#include <mach/mach_time.h>
#include <stdio.h>
#include "Phonebook.h"
// clang -O3 Phonebook.m -o Phonebook.bin -framework Foundation -fobjc-arc

@implementation Record
- (NSString*) Firstname { return First; }
- (NSString*) Lastname { return Last; }

- (id) init {
    if ( self = [super init]) {
      [self setFirst:First];
      [self setLast:Last];
    }
    return self;
}

- (NSComparisonResult)compare:(Record *)otherObject {
  NSComparisonResult FirstComp =
    [self->Last compare:otherObject->Last];
  if (FirstComp != NSOrderedSame)
    return FirstComp;

  return [self->First compare:otherObject->First];
}

- (void) setFirst: (NSString*)input { First = input; }
- (void) setLast:  (NSString*)input { Last = input; }
@end

int main(void) {

  NSArray *myArray = @[
    @"James", @"John", @"Robert", @"Michael", @"William", @"David", @"Richard", @"Joseph",
    @"Charles", @"Thomas", @"Christopher", @"Daniel", @"Matthew", @"Donald", @"Anthony",
    @"Paul", @"Mark", @"George", @"Steven", @"Kenneth", @"Andrew", @"Edward", @"Brian",
    @"Joshua", @"Kevin", @"Ronald", @"Timothy", @"Jason", @"Jeffrey", @"Gary", @"Ryan",
    @"Nicholas", @"Eric", @"Stephen", @"Jacob", @"Larry", @"Frank", @"Jonathan", @"Scott",
    @"Justin", @"Raymond", @"Brandon", @"Gregory", @"Samuel", @"Patrick", @"Benjamin",
    @"Jack", @"Dennis", @"Jerry", @"Alexander", @"Tyler", @"Douglas", @"Henry", @"Peter",
    @"Walter", @"Aaron", @"Jose", @"Adam", @"Harold", @"Zachary", @"Nathan", @"Carl",
    @"Kyle", @"Arthur", @"Gerald", @"Lawrence", @"Roger", @"Albert", @"Keith", @"Jeremy",
    @"Terry", @"Joe", @"Sean", @"Willie", @"Jesse", @"Ralph", @"Billy", @"Austin", @"Bruce",
    @"Christian", @"Roy", @"Bryan", @"Eugene", @"Louis", @"Harry", @"Wayne", @"Ethan",
    @"Jordan", @"Russell", @"Alan", @"Philip", @"Randy", @"Juan", @"Howard", @"Vincent",
    @"Bobby", @"Dylan", @"Johnny", @"Phillip", @"Craig"];

  NSMutableArray *PhoneBook = [[NSMutableArray alloc] init];

  int size = [myArray count];
  for (int i=0; i < size; i++) {
    for (int j=0; j < size; j++) {
      NSString *First = [myArray objectAtIndex: i];
      NSString *Last = [myArray objectAtIndex: j];
      Record *entry = [[Record alloc] init];
      [entry setFirst: First];
      [entry setLast: Last];
      [PhoneBook addObject: entry];
    }
  }

  uint64_t count = 0;
  uint64_t start = mach_absolute_time();
  for (unsigned i = 0; i < 100; i++) {
      NSMutableArray *NMA = [PhoneBook mutableCopy];
      [NMA sortUsingSelector: @selector(compare:)];
#if 0
      for (int i=0; i < size; i++) {
        Record *rec = [NMA objectAtIndex: i];
        printf("%s %s\n", [[rec Lastname] UTF8String], [[rec Firstname] UTF8String]);
      }
#endif
  }
  uint64_t delta = mach_absolute_time() - start;


  printf("%f ns\n",
 (double)delta);
  return 0;
}
