class WithStaticMemberFunc {
public:
  static int staticMemberFunc();
  typedef int (*Func)();
  static Func getStaticMemberFuncAddress();
};
