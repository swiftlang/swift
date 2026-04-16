namespace NS {
struct MyString;
}

NS::MyString makeMyString();

namespace NS {
struct MyString {
public:
  int val;

private:
  MyString();
  friend NS::MyString makeMyString();
};
} // namespace NS
