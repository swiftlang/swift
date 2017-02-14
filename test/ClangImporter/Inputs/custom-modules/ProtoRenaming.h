@interface GraphView

@end

@protocol GraphViewSource
@optional
- (void)doSomethingToGraphView:(nonnull GraphView *)view;
@end
