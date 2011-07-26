#include "string.h"
#include <cctype>

#include <iostream>

void
print(char c)
{
    if (std::isprint(c))
        std::cout << c;
    else
        std::cout << (unsigned)(unsigned char)c;
}

void
print(char16_t c)
{
    if (std::isprint(c))
        std::cout << (char)c;
    else
        std::cout << c;
}

void
print(char32_t c)
{
    if (std::isprint(c))
        std::cout << (char)c;
    else
        std::cout << c;
}

void
print(const char* s)
{
    std::cout << s;
}

void
print(const string& s)
{
    std::cout << static_cast<const char*>(s);
}

template <class T, std::size_t N>
void
print(const std::array<T, N>& a)
{
    std::cout << '{';
    if (N != 0)
    {
        print(a[0]);
        for (std::size_t i = 1; i < N; ++i)
        {
            print(", ");
            print(a[i]);
        }
    }
    print('}');
}

template <class T, class A0, class ...Args>
void
print(const T& t, const A0& a0, const Args& ...args)
{
    print(t);
    print(a0, args...);
}

int main()
{
    string s1("a1", tag());
    print(s1);
    std::cout << s1.size() << '\n';
    std::cout << s1.size_bytes() << '\n';
    string s2("b1", tag());
    print(s2);
    string s3("c1", tag());
    print(s3);
    string s4("d1", tag());
    print(s4);
    const char* s = "abcdef";
    string s5 = s;
    print(s5);
    std::cout << s5.size() << '\n';
    std::cout << s5.size_bytes() << '\n';
    s5 = s1;
    print(s5);
    print(s1);
    string s6 = s1;
    print(s1);
    print(s6);
    s1 = s3;
    string s7("abc\xEF\xAC\x84""def", tag());
    print(s7);
    std::cout << s7.size() << '\n';
    std::cout << s7.size_bytes() << '\n';
    for (string::range r(s7); !r.is_empty(); r.next())
        print(r.get(), ' ');
    std::cout << '\n';
}