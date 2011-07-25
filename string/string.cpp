#include "string.h"
#include <stdexcept>
#include <tuple>
#include <cstdlib>
#include <cstring>
#include <limits>

// Private implementation functions in unnamed namespace
namespace
{

// Counting and validation routines are optimized for ASCII

// Return the number of code points in the validated, null-terminated UTF-8
//   string.
std::size_t utf8_len(const char* p)
{
    std::size_t s = 0;
    for (; *p; ++s)
    {
        if ((*p & 0x80) == 0)
        {
            ++p;
            continue;
        }
        if ((*p & 0xE0) == 0xC0)
        {
            p += 2;
            continue;
        }
        if ((*p & 0xF0) == 0xE0)
        {
            p += 3;
            continue;
        }
        p += 4;
    }
    return s;
}

// Return tuple<number of code points, number of bytes> of the null-terminated
//   UTF-8 string.  Throw an exception if invalid UTF-8 is found.  Throw an
//   exception if the number of code points exceeds what can be represented by
//   ptrdiff_t.
std::tuple<std::size_t, std::size_t>
validate(const char* p)
{
    if (p == nullptr)
        throw std::runtime_error("invalid utf8");
    std::size_t num_char = 0;
    std::size_t num_bytes = 0;
    for (; *p; ++num_char)
    {
        if (num_char == std::numeric_limits<std::ptrdiff_t>::max() - 1)
            throw std::length_error("string too long");
        if ((*p & 0x80) == 0)
        {
            ++num_bytes;
            ++p;
            continue;
        }
        if ((*p & 0xE0) == 0xC0)
        {
            num_bytes += 2;
            ++p;
            if ((*p & 0xC0) != 0x80)
                throw std::runtime_error("invalid utf8");
            ++p;
            continue;
        }
        if ((*p & 0xF0) == 0xE0)
        {
            num_bytes += 3;
            ++p;
            if ((*p & 0xC0) != 0x80)
                throw std::runtime_error("invalid utf8");
            ++p;
            if ((*p & 0xC0) != 0x80)
                throw std::runtime_error("invalid utf8");
            ++p;
            continue;
        }
        if ((*p & 0xF8) != 0xF0)
            throw std::runtime_error("invalid utf8");
        num_bytes += 4;
        ++p;
        if ((*p & 0xC0) != 0x80)
            throw std::runtime_error("invalid utf8");
        ++p;
        if ((*p & 0xC0) != 0x80)
            throw std::runtime_error("invalid utf8");
        ++p;
        if ((*p & 0xC0) != 0x80)
            throw std::runtime_error("invalid utf8");
        ++p;
    }
    return std::make_tuple(num_char, num_bytes);
}

}  // unnamed namespace

string::string()
    : data_(nullptr)
{
}

string::~string()
{
    if (owns_)
        release();
}

string::string(const string& s)
    : data_(s.data_)
{
    if (owns_)
        retain();
}

string&
string::operator=(const string& s)
{
    if (s.owns_)
        s.retain();
    if (owns_)
        release();
    data_ = s.data_;
    return *this;
}

void
string::init_rom(const char* str, std::size_t N)
{
    data_ = str;
    validate(data_);
}

string::string(const char* s)
    : data_(nullptr)
{
    if (s != nullptr)
    {
        std::size_t num_bytes;
        std::size_t num_char;
        std::tie(num_char, num_bytes) = validate(s);
        // layout: refcount, num_chars, data, null terminator
        //   null terminator necessary only for printing
        //   (conversion to const char*), and can be easily removed here.
        data_ = (const char*)std::calloc(2*sizeof(std::size_t) + num_bytes + 1, 1);
        if (data_ == nullptr)
            throw std::bad_alloc();
        data_ += 2*sizeof(std::size_t);
        std::memcpy(const_cast<char*>(data_), s, num_bytes);
        const std::size_t N = sizeof(std::size_t) * CHAR_BIT - 1;
        count() = (std::size_t(num_char != num_bytes) << N) | num_char;
        owns_ = 1;
    }
}

string::operator const char*() const
{
    std::size_t const mask = std::size_t(-1) >> 1;
    return reinterpret_cast<const char*>(reinterpret_cast<std::size_t>(data_) & mask);
}

std::size_t
string::size_bytes() const
{
    if (data_ == nullptr)
        return 0;
    if (owns_)
    {
        const std::size_t rmask = ~(std::size_t(-1) >> 1);
        const std::size_t s = count();
        if (!(s & rmask))
            return s;
    }
    std::size_t s = 0;
    for (const char* p = static_cast<const char*>(*this); *p; ++p, ++s)
        ;
    return s;
}

std::size_t
string::size() const
{
    if (data_ == nullptr)
        return 0;
    if (owns_)
    {
        const std::size_t lmask = std::size_t(-1) >> 1;
        return count() & lmask;
    }
    return utf8_len(static_cast<const char*>(*this));
}

void
string::retain() const
{
    std::size_t& ref = *((std::size_t*)static_cast<const char*>(*this) - 2);
    __sync_add_and_fetch(&ref, 1);
}

void
string::release()
{
    std::size_t& ref = *((std::size_t*)static_cast<const char*>(*this) - 2);
    if (__sync_add_and_fetch(&ref, -1) == -1)
    {
        std::free(&ref);
        data_ = nullptr;
    }
}

string::range::range(const string& s)
    : s_(s),
      p_(static_cast<const char*>(s_)),
      indx_(0)
{
}

bool
string::range::is_empty() const
{
    return indx_ == s_.size();
}

void
string::range::next()
{
    if (indx_ == s_.size())
        throw std::out_of_range("range::next");
    ++indx_;
    if ((*p_ & 0x80) == 0)
        ++p_;
    else if ((*p_ & 0xE0) == 0xC0)
        p_ += 2;
    else if ((*p_ & 0xF0) == 0xE0)
        p_ += 3;
    else
        p_ += 4;
}

// Conversion routines optimized for ASCII

std::array<char, 4>
string::range::get_utf8() const
{
    if ((*p_ & 0x80) == 0)
        return std::array<char, 4>{p_[0], 0, 0, 0};
    if ((*p_ & 0xE0) == 0xC0)
        return std::array<char, 4>{p_[0], p_[1], 0, 0};
    if ((*p_ & 0xF0) == 0xE0)
        return std::array<char, 4>{p_[0], p_[1], p_[2], 0};
    return std::array<char, 4> {p_[0], p_[1], p_[2], p_[3]};
}

std::array<char16_t, 2>
string::range::get_utf16() const
{
    if ((p_[0] & 0x80) == 0)
        return std::array<char16_t, 2>{p_[0], 0};
    if ((p_[0] & 0xE0) == 0xC0)
        return std::array<char16_t, 2>{(char16_t(p_[0] & 0x1F) << 6) |
                                       (char16_t(p_[1] & 0x3F)), 0};
    if ((p_[0] & 0xF0) == 0xE0)
        return std::array<char16_t, 2>{(char16_t(p_[0] & 0x0F) << 12) |
                                       (char16_t(p_[1] & 0x3F) << 6)  |
                                       (char16_t(p_[2] & 0x3F)), 0};
    return std::array<char16_t, 2>
    {
        0xD800 | (((char16_t(p_[0] & 0x07) << 2)          |
                 (char16_t(p_[1] & 0x30) >> 4) - 1) << 6) |
                 (char16_t(p_[1] & 0x0F) << 2)            |
                 (char16_t(p_[2] & 0x30) >> 4),
        0xDC00 | (char16_t(p_[2] & 0x0F) << 6)   | char16_t(p_[3] & 0x3F)
    };
}

char32_t
string::range::get_utf32() const
{
    if ((p_[0] & 0x80) == 0)
        return char32_t(p_[0]);
    if ((p_[0] & 0xE0) == 0xC0)
        return (char32_t(p_[0] & 0x1F) << 6) | (char32_t(p_[1] & 0x3F));
    if ((p_[0] & 0xF0) == 0xE0)
        return (char32_t(p_[0] & 0x0F) << 12) | (char32_t(p_[1] & 0x3F) << 6)
                                              | (char32_t(p_[2] & 0x3F));
    return (char32_t(p_[0] & 0x07) << 18) | (char32_t(p_[1] & 0x3F) << 12)
                                          | (char32_t(p_[2] & 0x3F) << 6)
                                          | (char32_t(p_[3] & 0x3F));
}
