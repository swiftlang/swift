struct HasDeducingThis {
  int value;
  int deducingRef(this HasDeducingThis &self) { return self.value; }
};
