template <typename T>
struct StaticData {
    StaticData() {}
    StaticData(const StaticData &) = delete;
    StaticData(StaticData &&) = delete;
    StaticData &operator=(const StaticData &) = delete;
    StaticData &operator=(StaticData &&) = delete;

    T *operator->() const { return &value; }
    mutable T value;
};

struct TokensType {
    int token;
};

namespace ns {
StaticData<TokensType> Tokens;
}
