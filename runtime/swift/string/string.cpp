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

// Return the number of bytes in the UTF-8 string starting at p and holding
//   size_chars code points
std::size_t
count_bytes(const char* p, std::size_t size_chars)
{
    std::size_t num_bytes = 0;
    for (std::size_t i = 0; i < size_chars; ++i)
    {
        if ((*p & 0x80) == 0)
        {
            ++num_bytes;
            ++p;
            continue;
        }
        if ((*p & 0xE0) == 0xC0)
        {
            num_bytes += 2;
            p += 2;
            continue;
        }
        if ((*p & 0xF0) == 0xE0)
        {
            num_bytes += 3;
            p += 3;
            continue;
        }
        num_bytes += 4;
        p += 4;
    }
    return num_bytes;
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
        if (num_char == std::numeric_limits<std::int32_t>::max() - 1)
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

// Return number of code points of the null-terminated
//   UTF-8 string.  Throw an exception if invalid UTF-8 is found.  Throw an
//   exception if the number of code points exceeds what can be represented by
//   ptrdiff_t.
std::size_t
validate(const char* p, std::size_t N)
{
    if (p == nullptr)
        throw std::runtime_error("invalid utf8");
    std::size_t num_char = 0;
    for (std::size_t num_bytes = 0; num_bytes < N; ++num_char)
    {
        if (num_char == std::numeric_limits<std::int32_t>::max() - 1)
            throw std::length_error("string too long");
        if ((*p & 0x80) == 0)
        {
            ++num_bytes;
            ++p;
            continue;
        }
        if ((*p & 0xE0) == 0xC0)
        {
            if (num_bytes > N - 2)
                throw std::runtime_error("invalid utf8");
            num_bytes += 2;
            ++p;
            if ((*p & 0xC0) != 0x80)
                throw std::runtime_error("invalid utf8");
            ++p;
            continue;
        }
        if ((*p & 0xF0) == 0xE0)
        {
            if (num_bytes > N - 3)
                throw std::runtime_error("invalid utf8");
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
        if (num_bytes > N - 4)
            throw std::runtime_error("invalid utf8");
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
    return num_char;
}

}  // unnamed namespace

string::string()
    : data_(nullptr),
      word1_(0),
      offset_bytes_(0)
{
}

string::~string()
{
    if (owns_)
        release();
}

string::string(const string& s)
    : data_(s.data_),
      word1_(s.word1_),
      offset_bytes_(s.offset_bytes_)
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
    word1_ = s.word1_;
    offset_bytes_ = s.offset_bytes_;
    return *this;
}

void
string::init_rom(const char* str, std::size_t N)
{
    data_ = str;
    size_chars_ = validate(data_, N);
    is_not_ascii_ = size_chars_ != N;
    offset_bytes_ = 0;
}

string::string(const char* s)
    : data_(nullptr)
{
    if (s != nullptr)
    {
        std::size_t num_bytes;
        std::size_t num_char;
        std::tie(num_char, num_bytes) = validate(s);
        // layout: refcount, data, null terminator
        //   null terminator necessary only for printing
        //   (conversion to const char*), and can be easily removed here.
        // refcount == 0 means retain count is 1 (calloc implicitly retains)
        data_ = (const char*)std::calloc(sizeof(std::size_t) + num_bytes + 1, 1);
        if (data_ == nullptr)
            throw std::bad_alloc();
        data_ += sizeof(std::size_t);
        std::memcpy(const_cast<char*>(data_), s, num_bytes);
        owns_ = 1;
        is_not_ascii_ = num_char != num_bytes;
        size_chars_ = num_char;
        offset_bytes_ = 0;
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
    if (!is_not_ascii_)
        return size_chars_;
    const char* p = static_cast<const char*>(*this);
    return count_bytes(p, size_chars_);
}

std::size_t
string::size() const
{
    return size_chars_;
}

string
string::substring(std::size_t start_chars, std::size_t size_chars) const
{
    if (start_chars + size_chars > size_chars_)
        throw std::out_of_range("invalid substring");
    string result(*this);
    result.size_chars_ = size_chars;
    if (!is_not_ascii_)
    {
        result.offset_bytes_ = start_chars;
    }
    else
    {
        const char* p = static_cast<const char*>(*this);
        result.offset_bytes_ = count_bytes(p, start_chars);
        p += result.offset_bytes_;
        result.is_not_ascii_ = false;
        for (std::size_t i = 0; i < size_chars; ++i, ++p)
        {
            if ((*p & 0x80) != 0)
            {
                result.is_not_ascii_ = true;
                break;
            }
        }
    }
    return result;
}

void
string::retain() const
{
    std::size_t& ref = *((std::size_t*)static_cast<const char*>(*this) - 1);
    __sync_add_and_fetch(&ref, 1);
}

void
string::release()
{
    std::size_t& ref = *((std::size_t*)static_cast<const char*>(*this) - 1);
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

/*

Decided we didn't need these at the 2011-07-26 meeting

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
*/

char32_t
string::range::get() const
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
