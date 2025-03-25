#ifndef CppImpl_hpp
#define CppImpl_hpp

#include <stdio.h>

struct P { };
    
class C1 {
public:    
    P GetPrim() const { return P(); }
};
    
class C2: public C1 { };
    
class C3: public C2 { };

#endif /* CppImpl_hpp */
