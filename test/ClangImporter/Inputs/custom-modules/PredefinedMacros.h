#if !defined(__swift__)
# error "__swift__ not defined"
#elif __swift__ < 30000
# error "Why are you using such an old version of Swift?"
#elif __swift__ >= 810000
# error "Is Swift 81 out already? If so, please update this test."
#else
void swift3ReadyToGo();
#endif
