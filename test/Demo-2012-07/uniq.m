#import <Foundation/Foundation.h>
#include <stdio.h>

void uniq(NSString *path) {
  // Load the file into a string.
  NSString *contents = [[NSString alloc] initWithContentsOfFile: path];

  // Split into lines.
  NSArray *lines = [contents componentsSeparatedByString:@"\n"];

  // Count the items.
  NSMutableDictionary *counts = [[NSMutableDictionary alloc] init];
  for (NSString *ln in lines) {
    // Get the current count.
    NSNumber *countObject = [counts objectForKey:ln];
    int count = 0;
    if (countObject)
      count = countObject.intValue;

    // Update the count.
    [counts setObject:[[NSNumber alloc] initWithInt: (count + 1)] forKey:ln];
  }

  // Get the keys ordered by the count.
  NSArray *orderedKeys =
    [counts keysSortedByValueUsingComparator:^(NSString *a, NSString *b) {
        return -[a compare:b]; }];

  // Convert the items into an array of objects.
  for (NSString *key in orderedKeys) {
    NSString *message =
      [[NSString alloc] initWithFormat:@"%@: %@", [counts objectForKey:key], key];
    puts([message UTF8String]);
  }
}

int main() {
  @autoreleasepool {
    uniq(@"test.txt");
  }
}
