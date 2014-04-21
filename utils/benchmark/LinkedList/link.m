#import <Foundation/Foundation.h>
#include <mach/mach_time.h>
#include <stdio.h>

@interface MyNode:NSObject
@property int data;
@property(retain) MyNode *next;
@end

@implementation MyNode
@synthesize data, next;
@end


int main() {
  printf("Creating list\n");
  MyNode *head = [[MyNode alloc] init];

  for (int i = 0; i < 100; i++) {
    MyNode *next = [[MyNode alloc] init];
    [next setNext:[head next]];
    [next setData:i];
    [head setNext:next];
  }

  printf("Summing list\n");
  int sum = 0;
  uint64_t start = mach_absolute_time();
  MyNode *ptr = head;
  for (int i = 0; i < 100000; i++) {
    ptr = head;
    while (ptr) {
      sum += [ptr data];
      ptr = [ptr next];
    }
  }
  uint64_t delta = mach_absolute_time() - start;
  printf("%f ns\n", (double)delta);

  printf("sum = %d\n", sum);
  return 0;
}
