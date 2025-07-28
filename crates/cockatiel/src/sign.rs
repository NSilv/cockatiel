pub(crate) trait SignumInt {
  fn signum_i32(self) -> i32;
}

impl SignumInt for f32 {
  fn signum_i32(self) -> i32 {
    match self.signum() {
      1.0 => 1,
      -1.0 => -1,
      _ => panic!("unexpected result from signum"),
    }
  }
}
