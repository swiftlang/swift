#ifndef TEST_INTEROP_CXX_STATIC_INPUTS_INLINE_STATIC_MEMBER_VAR_H
#define TEST_INTEROP_CXX_STATIC_INPUTS_INLINE_STATIC_MEMBER_VAR_H

inline static int init() { return 42; }

class WithInlineStaticMember {
  public:
    inline static int staticMember = 12;
    inline static int staticMemberInitializedAtRuntime = init();

    static int getStaticMemberFromCxx();
    static int *getStaticMemberAddress()
        __attribute__((swift_attr("import_unsafe")));
    static void setStaticMemberFromCxx(int);
};

#endif
