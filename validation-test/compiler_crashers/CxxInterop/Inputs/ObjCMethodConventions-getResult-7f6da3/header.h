struct ArcStruct {
  __strong id obj;
};

@protocol ArcStructProvider
- (struct ArcStruct)makeArcStruct;
@end
