#ifndef TEST_INTEROP_CXX_STATIC_INPUTS_INLINE_STATIC_MEMBER_VAR_H
#define TEST_INTEROP_CXX_STATIC_INPUTS_INLINE_STATIC_MEMBER_VAR_H

inline static int init() { return 42; }

class WithInlineStaticMember {
  public:
    inline static int staticMember = 12;
    //TODO needs C++ stdlib symbols, fix after apple/swift#30914 is merged.
    // inline static int staticMemberInitializedAtRuntime = init();

    static int getStaticMemberFromCxx();
    static int *getStaticMemberAddress();
    static void setStaticMemberFromCxx(int);
};

#endif
