@interface NSSomeClass
  -(instancetype)init;
@end

// Extension, inspired by UIKit UIViewController.h
@interface NSSomeClass (UIContainerViewControllerCallbacks)

- (void)didMoveToParentViewController:(NSSomeClass *)parent;

@end
