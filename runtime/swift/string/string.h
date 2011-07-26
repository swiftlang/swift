#ifndef STRING_H
#define STRING_H

// This is a rough prototype string class referred to as S3 in the strings.html
// document.
// 
// The string sports a primative "range" class instead of traditional C++
// iterators. The range offers 1 view of the contents:  UTF-32.

// This prototype does not yet draw a distinction between recoverable errors and
//   non-recoverable errors.  Everything is an exception.  That will surely
//   change.

#include <cstddef>
#include <climits>
#include <cstdint>

// A hack to tell the constructor the argument is pointing into ROM.  Without
// this hack, string literals are drawn to the wrong constructor.
struct tag {};

// string invariant:  Points to null, or points to valid UTF-8 characters.
// The number of code points can be represented by 31 bits.
// A string with an internal null ptr represents an empty string.
// is_not_ascii_ is true if the string contains a non-ascii character.

class string
{
    union
    {
        // Need help knowing which bit I can steal from data_ pointer.  Unless
        //   swift promises to align character data in rom, this pointer is
        //   1-byte-aligned.
        // Currently stealing high bit.  This is scary on 32 bit platform.
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
    union
    {
        std::uint32_t word1_;
        struct
        {
            std::uint32_t is_not_ascii_ : 1;
            std::uint32_t size_chars_ : 31;
        };
    };
    std::uint32_t offset_bytes_;

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
        void next();                       // not in empty state
        // observers
        bool is_empty() const;
        char32_t get() const;              // not in empty state
    };

    string();
    ~string();
    string(const string& s);
    string& operator=(const string& s);
    // What constructors do we want?
    // This one currently models pointer into rom and C++ layout
    template <std::size_t N>
        string(const char (&str)[N], tag)
            {init_rom(str, N-1);}  // don't count trailing null
    // This one currently models dynamic ownership, and swift layout
    string(const char* s);

    // For printing, for now.
    //   Conversion need not have this form.  However conversion should have
    //      a generic form.  to_string() isn't generic.
    explicit operator const char*() const;

    std::size_t size() const;        // number of code points
    std::size_t size_bytes() const;  // number of bytes

    string substring(std::size_t start, std::size_t size) const;

private:

    void init_rom(const char* str, std::size_t N);

    const std::size_t& count() const
        {return *(reinterpret_cast<const std::size_t*>
                                        (static_cast<const char*>(*this)) - 1);}
    std::size_t& count()
            {return const_cast<std::size_t&>(((const string*)this)->count());}

    void retain() const;
    void release();

    friend class range;
};

#endif  // STRING_H
