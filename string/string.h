#ifndef STRING_H
#define STRING_H

// This is a rough prototype string class referred to as S1+ in the strings.html
// document.  It is a descriminated union of two pointer types.  One pointer
// points into ROM.  The other pointer points to a ref-counted block of
// dynamically allocated memory.
// 
// The string sports a primative "range" class instead of traditional C++
// iterators. The range offers 3 views of the contents:  UTF-8, UTF-16 and
// UTF-32.

// This prototype does not accurately model the string data layout in rom.  It
//   does point into rom to simulate the memory management.  But it points to
//   a C++ string literal in rom instead of a swift literal in rom.  The swift
//   literal in rom will have more data, such as the number of bytes, and/or
//   the number of code points.

// To support embedded null characters in a string, we need to store the number
//   of bytes in a string.  Otherwise to find the number of bytes in a
//   (non-ascii) UTF-8 string we need to actually decode it to find the number
//   of bytes.

// This prototype does not yet draw a distinction between recoverable errors and
//   non-recoverable errors.  Everything is an exception.  That will surely
//   change.

#include <cstddef>
#include <climits>
#include <array>

// A hack to tell the constructor the argument is pointing into ROM.  Without
// this hack, string literals are drawn to the wrong constructor.
struct tag {};

// string invariant:  Points to null, or points to valid UTF-8 characters.
// The number of code points can be represented by ptrdiff_t.
// A string with an internal null ptr represents an empty string.

class string
{
    union
    {
        const char* data_;
        struct
        {
#if _LIBCPP_BIG_ENDIAN
            std::size_t owns_ : 1;
#endif
            std::size_t _ : sizeof(char*)*CHAR_BIT-1;
#if _LIBCPP_LITTLE_ENDIAN
            std::size_t owns_ : 1;
#endif
        };
    };

    void init_rom(const char* str, std::size_t N);

    const std::size_t& count() const
        {return *(reinterpret_cast<const std::size_t*>
                                        (static_cast<const char*>(*this)) - 1);}
    std::size_t& count()
            {return const_cast<std::size_t&>(((const string*)this)->count());}

    void retain() const;
    void release();
public:
    // Example range view
    //   range invariant: s_ points to a valid string.
    //                    p_ points into s_'s data buffer at the index_'th
    //                       code point.
    //                    index_ is in [0, s_.size()).
    class range
    {
        const string& s_;
        const char* p_;
        std::size_t indx_;
    public:
        // store reference to string
        explicit range(const string& s);

        // increment to next code point
        void next();                                // not in empty state
        // observers
        bool is_empty() const;
        std::array<char, 4> get_utf8() const;       // not in empty state
        std::array<char16_t, 2> get_utf16() const;  // not in empty state
        char32_t get_utf32() const;                 // not in empty state
    };

    string();
    ~string();
    string(const string& s);
    string& operator=(const string& s);
    // What constructors do we want?
    // This one currently models pointer into rom and C++ layout
    template <std::size_t N>
        string(const char (&str)[N], tag)
            {init_rom(str, N);}
    // This one currently models dynamic ownership, and swift layout
    string(const char* s);

    // For printing, for now.
    //   Conversion need not have this form.  However conversion should have
    //      a generic form.  to_string() isn't generic.
    explicit operator const char*() const;

    std::size_t size() const;        // number of code points
    std::size_t size_bytes() const;  // number of bytes

    friend class range;
};

#endif  // STRING_H
