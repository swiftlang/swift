struct Buddy {};

template <typename T>
struct BuddyTemplate {};

struct NoFriend {
  int x;
};

struct FriendClass {
  int x;
  friend struct Buddy;
};

struct FriendFunction {
  int x;
  friend bool areEqual(const FriendFunction &, const FriendFunction &);
};

struct FriendFunctionDefinition {
  int x;
  friend int getX(const FriendFunctionDefinition &self) { return self.x; }
};

struct FriendTemplateSpecialization {
  int x;
  friend struct BuddyTemplate<int>;
};

struct FriendWholeTemplate {
  int x;

  template <typename>
  friend struct BuddyTemplate;
};

struct HasFriendlyMember {
  FriendClass member;
};
