template <typename T> struct Box {
  T value;
};
inline Box<bool> getBox() { return {true}; }
