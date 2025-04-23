fn x () -> i32 { 1 }

fn y (x_ : i32) -> bool { x() == x_ }

fn main() { y(2); }
