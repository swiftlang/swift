#import <Foundation/Foundation.h>
#include <stdio.h>

void uniq(NSString *path) {
  // Load the file into a string.
  NSStringEncoding encoding;
  NSError *error;
  NSString *contents = [[NSString alloc] initWithContentsOfFile: path
                                         usedEncoding: &encoding
                                         error: &error];

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
    [counts setObject:@(count + 1) forKey:ln];
  }

  // Get the keys ordered by the count.
  NSArray *orderedKeys =
    [counts keysSortedByValueUsingComparator:^(NSString *a, NSString *b) {
        return -[a compare:b]; }];

  // Convert the items into an array of objects.
  for (NSString *key in orderedKeys) {
    NSNumber *count = [counts objectForKey:key];
    printf("%d: %s\n", count.intValue, [key UTF8String]);
  }
}

int main(int argc, char **argv) {
  @autoreleasepool {
    uniq([[NSString alloc] initWithUTF8String: argv[1]]);
  }
}
