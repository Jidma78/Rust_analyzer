fn swap(x : &mut i32, y : &mut i32) {
    let t = *x;
    *x = *y;
    *y = t;
}

fn main() {
    let mut x = &1;
    let mut y = &2;
    swap(x, y);
}
