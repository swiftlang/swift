@protocol Order2_ConsistentBaseProto
- (void)consistent;
@end

@protocol Order4_ConsistentBaseProto
- (void)consistent;
@end

@protocol Order1_FickleBaseProto
- (void)fickle;
@optional
- (void)extraFickle;
@end

@protocol Order3_FickleBaseProto
- (void)fickle;
@optional
- (void)extraFickle;
@end

@protocol Order5_FickleBaseProto
- (void)fickle;
@optional
- (void)extraFickle;
@end

// The actual order here is determined by the protocol names.
#if EXTRA_PROTOCOL_FIRST
@protocol SubProto <Order1_FickleBaseProto, Order2_ConsistentBaseProto, Order4_ConsistentBaseProto>
@end
#elif EXTRA_PROTOCOL_MIDDLE
@protocol SubProto <Order2_ConsistentBaseProto, Order3_FickleBaseProto, Order4_ConsistentBaseProto>
@end
#elif EXTRA_PROTOCOL_LAST
@protocol SubProto <Order2_ConsistentBaseProto, Order4_ConsistentBaseProto, Order5_FickleBaseProto>
@end
#elif NO_EXTRA_PROTOCOLS
@protocol SubProto <Order2_ConsistentBaseProto, Order4_ConsistentBaseProto>
@end
#else
# error "Missing -D flag"
#endif
